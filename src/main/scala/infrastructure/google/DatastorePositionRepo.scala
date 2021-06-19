package io.softwarechain.cryptojournal
package infrastructure.google

import domain.model.{UserId, WalletAddress}
import domain.position.{Position, PositionRepo}
import infrastructure.google.DatastorePositionRepo.DemoPositionsKind

import com.google.cloud.Timestamp
import com.google.cloud.datastore.{Datastore, DoubleValue, Entity, EntityValue, ListValue, StringValue, TimestampValue}
import zio.{Has, Task, URLayer}
import zio.logging.{Logger, Logging}

import java.util.UUID
import scala.jdk.CollectionConverters.SeqHasAsJava

final case class DatastorePositionRepo (datastore: Datastore, logger: Logger[String]) extends PositionRepo {
  override def save(userId: UserId, address: WalletAddress, list: List[Position]): Task[Unit] = {
    val entities = list.map(pos => positionToEntity(pos, userId, address, DemoPositionsKind))
    Task(datastore.add(entities: _*)).ignore
  }

  private val positionToEntity: (Position, UserId, WalletAddress, String) => Entity = (position, userId, address, kind) => {
    val entries = position.entries.map { entry =>
      EntityValue.of(
        Entity.newBuilder(datastore.newKeyFactory().newKey(UUID.randomUUID().toString))
          .set("type", StringValue.of(entry.`type`.toString))
          .set("value", DoubleValue.of(entry.value.amount.doubleValue))
          .set("valueCurrency", StringValue.of(entry.value.currency))
          .set("fee", DoubleValue.of(entry.fee.amount.doubleValue))
          .set("feeCurrency", StringValue.of(entry.fee.currency))
          .build()
      )
    }

    var builder = Entity.newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
      .set("userId", StringValue.of(userId.value))
      .set("address", StringValue.of(address.value))
      .set("currency", StringValue.of(position.coin))
      .set("state", StringValue.of(position.state.toString))
      .set("openedAt", TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano)))
      .set("entries", ListValue.newBuilder().set(entries.asJava).setExcludeFromIndexes(true).build())

    if(position.closedAt.isDefined) {
      val closedAt = position.closedAt.get
      builder = builder.set("closedAt", TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(closedAt.getEpochSecond, closedAt.getNano)))
    }

    builder.build()
  }
}

object DatastorePositionRepo {
  val DemoPositionsKind = "DemoPositions"

  lazy val layer: URLayer[Has[Datastore] with Logging, Has[PositionRepo]] = (DatastorePositionRepo(_, _)).toLayer
}
