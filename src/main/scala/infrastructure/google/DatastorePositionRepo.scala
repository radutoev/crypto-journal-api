package io.softwarechain.cryptojournal
package infrastructure.google

import domain.model._
import domain.position.error._
import domain.position.{ Position, PositionEntry, PositionRepo }
import domain.position.Position.PositionIdPredicate
import infrastructure.google.DatastorePositionRepo._
import util.tryOrLeft
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore._
import eu.timepit.refined
import eu.timepit.refined.types.numeric.PosInt
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, Task, URLayer, ZIO }

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastorePositionRepo(datastore: Datastore, logger: Logger[String], clock: Clock.Service)
    extends PositionRepo {

  override def save(address: WalletAddress, positions: List[Position]): Task[Unit] = {
    @inline
    def saveEntities(list: List[Entity]) =
      Task(datastore.newTransaction())
        .bracket(txn => Task(txn.rollback()).when(txn.isActive).ignore) { txn =>
          Task {
            val addressLockKey = datastore.newKeyFactory().setKind("AddressImport").newKey(address.value)
            txn.put(Entity.newBuilder(addressLockKey).build())
            txn.put(list: _*)
            txn.delete(addressLockKey)
            txn.commit()
          }
        }
        .tapError(throwable =>
          logger.error(s"Error importing positions for ${address.value}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle transactions response when doing error handling.

    val entities = positions.map(pos => positionToEntity(pos, address, Positions)).grouped(23).toList

    for {
      _       <- ZIO.foreach(entities)(saveEntities).ignore
      instant <- clock.instant
      _       <- upsertPositionSyncJob(address)(instant)
      _       <- logger.info(s"Finished importing positions for address ${address.value}")
    } yield ()
  }

  override def getPositions(address: WalletAddress)(implicit count: PosInt): Task[List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(Positions)
      .setFilter(PropertyFilter.eq("address", address.value))
      .addOrderBy(OrderBy.asc("openedAt"))
      .setLimit(count.value)
      .build()

    executeQuery(query)
      .map(results => results.asScala.toList.map(entityToPosition).collect { case Right(position) => position })
  }

  //TODO How to include end?
  override def getPositions(address: WalletAddress, timeInterval: TimeInterval): Task[List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(Positions)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.ge(
            "openedAt",
            Timestamp.ofTimeSecondsAndNanos(timeInterval.start.getEpochSecond, timeInterval.start.getNano)
          )
        )
      )
      .addOrderBy(OrderBy.asc("openedAt"))
      .build()
    executeQuery(query)
      .map(results => results.asScala.toList.map(entityToPosition).collect { case Right(position) => position })
  }

  override def exists(address: WalletAddress): Task[Boolean] =
    executeQuery(
      Query.newKeyQueryBuilder().setKind(PositionSyncJob).setFilter(PropertyFilter.eq("address", address.value)).build()
    ).tapError(err => logger.warn(err.toString))
      .map(results => results.asScala.nonEmpty)

  private def upsertPositionSyncJob(address: WalletAddress)(implicit instant: Instant) = Task {
    datastore.put(
      Entity
        .newBuilder(datastore.newKeyFactory().setKind(PositionSyncJob).newKey(address.value))
        .set("updatedAt", Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano))
        .build()
    )
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private val positionToEntity: (Position, WalletAddress, String) => Entity =
    (position, address, kind) => {
      val entries = position.entries.map { entry =>
        EntityValue.of(
          Entity
            .newBuilder(datastore.newKeyFactory().setKind(Positions).newKey(UUID.randomUUID().toString))
            .set("type", StringValue.of(entry.`type`.toString))
            .set("value", DoubleValue.of(entry.value.amount.doubleValue))
            .set("valueCurrency", StringValue.of(entry.value.currency))
            .set("fee", DoubleValue.of(entry.fee.amount.doubleValue))
            .set("feeCurrency", StringValue.of(entry.fee.currency))
            .set(
              "timestamp",
              TimestampValue
                .of(Timestamp.ofTimeSecondsAndNanos(entry.timestamp.getEpochSecond, entry.timestamp.getNano))
            )
            .set("hash", entry.txHash.value)
            .build()
        )
      }

      var builder = Entity
        .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
        .set("address", StringValue.of(address.value))
        .set("currency", StringValue.of(position.currency))
        .set("state", StringValue.of(position.state.toString))
        .set(
          "openedAt",
          TimestampValue
            .of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano))
        )
        .set("entries", ListValue.newBuilder().set(entries.asJava).build())

      if (position.closedAt.isDefined) {
        val closedAt = position.closedAt.get
        builder = builder.set(
          "closedAt",
          TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(closedAt.getEpochSecond, closedAt.getNano))
        )
      }

      builder.build()
    }

  private val entityToPosition: Entity => Either[InvalidRepresentation, Position] = entity => {

    (for {
      id <- tryOrLeft(entity.getKey.getName, InvalidRepresentation("Entity has no key name"))
             .flatMap(rawIdStr =>
               refined
                 .refineV[PositionIdPredicate](rawIdStr)
                 .left
                 .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
             )
      currency <- tryOrLeft(
                   entity.getString("currency"),
                   InvalidRepresentation("Invalid value currency representation")
                 )
      state <- tryOrLeft(entity.getString("state"), InvalidRepresentation("Invalid value currency representation"))
                .flatMap(State.apply(_).left.map(InvalidRepresentation))
      openedAt <- tryOrLeft(
                   Instant.ofEpochSecond(entity.getTimestamp("openedAt").getSeconds),
                   InvalidRepresentation("Invalid openedAt representation")
                 )
      entries <- tryOrLeft(
                  entity
                    .getList[EntityValue]("entries")
                    .asScala
                    .map(entryToPositionEntry)
                    .collect { case Right(entry) => entry }
                    .toList,
                  InvalidRepresentation("Invalid entries representation")
                )
    } yield Position(currency, state, openedAt, None, entries, None, Some(id))).map { position =>
      tryOrLeft(Instant.ofEpochSecond(entity.getTimestamp("closedAt").getSeconds), "")
        .fold(_ => position, closedAt => position.copy(closedAt = Some(closedAt)))
    }
  }

  private val entryToPositionEntry: EntityValue => Either[InvalidRepresentation, PositionEntry] = e => {
    val entity = e.get()
    for {
      entryType <- tryOrLeft(
                    TransactionType(entity.getString("type")),
                    InvalidRepresentation("Invalid type representation")
                  )
      value <- tryOrLeft(entity.getDouble("value"), InvalidRepresentation("Invalid value representation"))
      currency <- tryOrLeft(
                   entity.getString("valueCurrency"),
                   InvalidRepresentation("Invalid value currency representation")
                 )
      feeValue <- tryOrLeft(entity.getDouble("fee"), InvalidRepresentation("Invalid fee representation"))
      feeCurrency <- tryOrLeft(
                      entity.getString("feeCurrency"),
                      InvalidRepresentation("Invalid fee currency representation")
                    )
      timestamp <- tryOrLeft(
                    Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds),
                    InvalidRepresentation("Invalid timestamp representation")
                  )
      hash <- tryOrLeft(entity.getString("hash"), InvalidRepresentation("Invalid hash representation"))
               .flatMap(value => TransactionHash(value).left.map(InvalidRepresentation))
    } yield PositionEntry(
      entryType,
      FungibleData(value, currency),
      FungibleData(feeValue, feeCurrency),
      timestamp,
      hash
    )
  }
}

object DatastorePositionRepo {
  lazy val layer: URLayer[Has[Datastore] with Logging with Clock, Has[PositionRepo]] =
    (DatastorePositionRepo(_, _, _)).toLayer

  /* Tables */

  val Positions = "Position"

  //Maintains state of the awareness of the system regarding wallets.
  // Each wallet has a single entry, and metadata information regarding the latest blockchain sync.
  val PositionSyncJob: String = "PositionSyncJob"
}
