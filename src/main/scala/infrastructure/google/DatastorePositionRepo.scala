package io.softwarechain.cryptojournal
package infrastructure.google

import domain.model._
import domain.position.error._
import domain.position.{Position, PositionEntry, PositionRepo}
import infrastructure.google.DatastorePositionRepo._
import util.tryOrLeft

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore._
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer}

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastorePositionRepo(datastore: Datastore, logger: Logger[String]) extends PositionRepo {

  override def save(userId: UserId, address: WalletAddress, list: List[Position]): Task[Unit] = {
    val entities = list.map(pos => positionToEntity(pos, userId, address, Positions))
    Task(datastore.add(entities: _*)).catchAllCause(cause => logger.error(s"Unable to save positions: ${cause}")).ignore
  }

  override def getPositions(userId: UserId, address: WalletAddress): Task[List[Position]] =
    Task(
      datastore.run(
        Query
          .newEntityQueryBuilder()
          .setKind(Positions)
          .setFilter(
            CompositeFilter.and(PropertyFilter.eq("userId", userId.value), PropertyFilter.eq("address", address.value))
          )
          .addOrderBy(OrderBy.asc("openedAt"))
          .build(),
        Seq.empty[ReadOption]: _*
      )
    )
      .tapError(throwable => logger.warn(throwable.getMessage))
      .map(results => results.asScala.toList.map(entityToPosition).collect { case Right(position) => position })

  override def exists(address: WalletAddress): Task[Boolean] = {
    executeQuery(Query.newKeyQueryBuilder().setKind(PositionSyncJob).setFilter(PropertyFilter.eq("address", address.value)).build())
      .tapError(err => logger.warn(err.toString))
      .map(results => results.asScala.nonEmpty)
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] = {
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
  }

  private val positionToEntity: (Position, UserId, WalletAddress, String) => Entity =
    (position, userId, address, kind) => {
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
            .build()
        )
      }

      var builder = Entity
        .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
        .set("address", StringValue.of(address.value))
        .set("currency", StringValue.of(position.coin))
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
    } yield Position(currency, state, openedAt, None, entries)).map { position =>
      tryOrLeft(Instant.ofEpochSecond(entity.getTimestamp("closedAt").getSeconds), "")
        .fold(_ => position, closedAt => position.copy(closedAt = Some(closedAt)))
    }
  }

  //TODO Add the transaction hash as well.
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
    } yield PositionEntry(entryType, FungibleData(value, currency), FungibleData(feeValue, feeCurrency), timestamp)
  }
}

object DatastorePositionRepo {
  lazy val layer: URLayer[Has[Datastore] with Logging, Has[PositionRepo]] = (DatastorePositionRepo(_, _)).toLayer

  /* Tables */

  val Positions = "Positions"

  //Maintains state of the awareness of the system regarding wallets.
  // Each wallet has a single entry, and metadata information regarding the latest blockchain sync.
  val PositionSyncJob: String = "PositionSyncJob"
}
