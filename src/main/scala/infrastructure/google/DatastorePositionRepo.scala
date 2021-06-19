package io.softwarechain.cryptojournal
package infrastructure.google

import domain.model.{FungibleData, State, TransactionType, UserId, WalletAddress}
import domain.position.{Position, PositionEntry, PositionRepo}
import domain.position.error._
import infrastructure.google.DatastorePositionRepo.DemoPositionsKind
import util.tryOrLeft

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore.{Datastore, DoubleValue, Entity, EntityValue, FullEntity, ListValue, Query, ReadOption, StringValue, TimestampValue}
import zio.{Has, Task, URLayer}
import zio.logging.{Logger, Logging}

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastorePositionRepo(datastore: Datastore, logger: Logger[String]) extends PositionRepo {
  override def save(userId: UserId, address: WalletAddress, list: List[Position]): Task[Unit] = {
    val entities = list.map(pos => positionToEntity(pos, userId, address, DemoPositionsKind))
    Task(datastore.add(entities: _*)).ignore
  }

  override def getPositions(userId: UserId, address: WalletAddress): Task[List[Position]] =
    Task(
      datastore.run(
        Query
          .newEntityQueryBuilder()
          .setKind(DemoPositionsKind)
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

  private val positionToEntity: (Position, UserId, WalletAddress, String) => Entity =
    (position, userId, address, kind) => {
      val entries = position.entries.map { entry =>
        EntityValue.of(
          Entity
            .newBuilder(datastore.newKeyFactory().newKey(UUID.randomUUID().toString))
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
        .set("userId", StringValue.of(userId.value))
        .set("address", StringValue.of(address.value))
        .set("currency", StringValue.of(position.coin))
        .set("state", StringValue.of(position.state.toString))
        .set(
          "openedAt",
          TimestampValue
            .of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano))
        )
        .set("entries", ListValue.newBuilder().set(entries.asJava).setExcludeFromIndexes(true).build())

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

  private val entryToPositionEntry: EntityValue => Either[InvalidRepresentation, PositionEntry] = e => {
    val entity = e.get()
    for {
      entryType <- tryOrLeft(
                    TransactionType(entity.getString("value")),
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
  val DemoPositionsKind = "DemoPositions"

  lazy val layer: URLayer[Has[Datastore] with Logging, Has[PositionRepo]] = (DatastorePositionRepo(_, _)).toLayer
}
