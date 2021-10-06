package io.softwarechain.cryptojournal
package infrastructure.google

import config.DatastoreConfig
import domain.model._
import domain.position.Position.{ PositionEntryIdPredicate, PositionId, PositionIdPredicate }
import domain.position.error._
import domain.position.{ Position, PositionEntry, PositionRepo }
import util.{ tryOrLeft, InstantOps }
import vo.filter.PositionFilter

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore._
import eu.timepit.refined
import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastorePositionRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  logger: Logger[String]
) extends PositionRepo {

  override def save(address: WalletAddress, positions: List[Position]): Task[Unit] = {
    @inline
    def saveEntities(list: List[Entity]) =
      Task(datastore.newTransaction())
        .bracket(txn => Task(txn.rollback()).when(txn.isActive).ignore) { txn =>
          Task {
            val addressLockKey = datastore.newKeyFactory().setKind(datastoreConfig.address).newKey(address.value)
            txn.put(Entity.newBuilder(addressLockKey).build())
            txn.put(list: _*)
            txn.delete(addressLockKey)
            txn.commit()
          }
        }
        .tapError(throwable =>
          logger.error(s"Error saving positions for ${address.value}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle transactions response when doing error handling.

    val entities = positions.map(pos => positionToEntity(pos, address, datastoreConfig.position)).grouped(23).toList

    if (positions.isEmpty) {
      logger.debug(s"No positions to save for ${address.value}")
    } else {
      for {
        _ <- ZIO.foreach(entities)(saveEntities).ignore
        _ <- logger.info(s"Finished saving positions for address ${address.value}")
      } yield ()
    }
  }

  override def getPositions(
    address: WalletAddress
  )(positionFilter: PositionFilter): IO[PositionError, List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
//          PropertyFilter.ge("openedAt", positionFilter.interval.start.toDatastoreTimestamp()), //doesn't work because datastore filters.
          PropertyFilter.le("openedAt", positionFilter.interval.end.toDatastoreTimestamp())
        )
      )
      .addOrderBy(OrderBy.desc("openedAt"))
      .setLimit(positionFilter.count)
      .build()
    doFetchPositions(address, query)
  }

  override def getPositions(address: WalletAddress, startFrom: Instant): IO[PositionError, List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.gt(
            "openedAt",
            Timestamp.ofTimeSecondsAndNanos(startFrom.getEpochSecond, startFrom.getNano)
          )
        )
      )
      .addOrderBy(OrderBy.asc("openedAt"))
      .build()
    doFetchPositions(address, query)
  }

  override def getPositions(address: WalletAddress, state: State): IO[PositionError, List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.eq("state", state.toString)
        )
      )
      .addOrderBy(OrderBy.asc("openedAt"))
      .build()
    doFetchPositions(address, query)
  }

  private def doFetchPositions(address: WalletAddress, query: EntityQuery): IO[PositionsFetchError, List[Position]] =
    executeQuery(query)
      .map(Some(_))
      .catchSome {
        case e: DatastoreException if e.getMessage.contains("no matching index found") => UIO.none
      }
      .mapBoth(
        _ => PositionsFetchError(address),
        resultsOpt =>
          resultsOpt.fold[List[Position]](List.empty)(results =>
            results.asScala.toList.map(entityToPosition).collect { case Right(position) => position }
          )
      )

  override def getPosition(positionId: PositionId): IO[PositionError, Position] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.position).newKey(positionId.value)
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(PropertyFilter.eq("__key__", key))
      .build()
    executeQuery(query)
      .mapError(throwable => PositionFetchError(positionId, throwable))
      .flatMap { queryResult =>
        val results = queryResult.asScala
        if (results.nonEmpty) {
          ZIO.fromEither(entityToPosition(results.next()))
        } else {
          ZIO.fail(PositionNotFound(positionId))
        }
      }
  }

  override def exists(address: WalletAddress): Task[Boolean] = ???
//    executeQuery(
//      Query
//        .newKeyQueryBuilder()
//        .setKind(datastoreConfig.checkpoint)
//        .setFilter(
//          PropertyFilter
//            .eq("__key__", datastore.newKeyFactory().setKind(datastoreConfig.checkpoint).newKey(address.value))
//        )
//        .build()
//    ).map(results => results.asScala.nonEmpty)

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private val positionToEntity: (Position, WalletAddress, String) => Entity =
    (position, address, kind) => {
      val entries = position.entries.map { entry =>
        EntityValue.of(
          Entity
            .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
            .set("type", StringValue.of(entry.`type`.toString))
            .set("value", DoubleValue.of(entry.value.amount.doubleValue))
            .set("valueCurrency", StringValue.of(entry.value.currency.value))
            .set("fee", DoubleValue.of(entry.fee.amount.doubleValue))
            .set("feeCurrency", StringValue.of(entry.fee.currency.value))
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
        .set("currency", StringValue.of(position.currency.value))
        .set("state", StringValue.of(position.state.toString))
        .set(
          "openedAt",
          TimestampValue
            .of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano))
        )
        .set("entries", ListValue.newBuilder().set(entries.asJava).build())

      if (position.closedAt().isDefined) {
        val closedAt = position.closedAt().get
        builder = builder.set(
          "closedAt",
          TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(closedAt.getEpochSecond, closedAt.getNano))
        )
      }

      builder.build()
    }

  private val entityToPosition: Entity => Either[InvalidRepresentation, Position] = entity => {

    for {
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
                 ).flatMap(refineV[CurrencyPredicate](_).left.map(InvalidRepresentation))
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
    } yield Position(currency, openedAt, entries, id = Some(id))
  }

  private val entryToPositionEntry: EntityValue => Either[InvalidRepresentation, PositionEntry] = e => {
    val entity = e.get()
    for {
      id <- tryOrLeft(entity.getKey().asInstanceOf[Key].getName, InvalidRepresentation("Entry has no key name"))
             .flatMap(rawIdStr =>
               refined
                 .refineV[PositionEntryIdPredicate](rawIdStr)
                 .left
                 .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
             )
      entryType <- tryOrLeft(
                    TransactionType(entity.getString("type")),
                    InvalidRepresentation("Invalid type representation")
                  )
      value <- tryOrLeft(entity.getDouble("value"), InvalidRepresentation("Invalid value representation"))
      currency <- tryOrLeft(
                   entity.getString("valueCurrency"),
                   InvalidRepresentation("Invalid value currency representation")
                 ).flatMap(refined.refineV[CurrencyPredicate](_).left.map(InvalidRepresentation))
      feeValue <- tryOrLeft(entity.getDouble("fee"), InvalidRepresentation("Invalid fee representation"))
      feeCurrency <- tryOrLeft(
                      entity.getString("feeCurrency"),
                      InvalidRepresentation("Invalid fee currency representation")
                    ).flatMap(refined.refineV[CurrencyPredicate](_).left.map(InvalidRepresentation))
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
      hash,
      id = Some(id)
    )
  }
}

object DatastorePositionRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging with Clock, Has[PositionRepo]] =
    (DatastorePositionRepo(_, _, _)).toLayer
}
