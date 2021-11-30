package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.{ContextId, ContextIdPredicate}
import domain.position.error._
import util.{InstantOps, tryOrLeft}
import vo.pagination.{CursorPredicate, PaginationContext}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore._
import eu.timepit.refined
import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

import java.time.Instant
import scala.jdk.CollectionConverters.{IteratorHasAsScala, SeqHasAsJava}

final case class DatastorePaginationRepo(datastore: Datastore,
                                         datastoreConfig: DatastoreConfig,
                                         clock: Clock.Service,
                                         logger: Logger[String])  {
  def getPaginationContext(contextId: ContextId): IO[MarketPlayError, PaginationContext] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.paginationContext).newKey(contextId.value)
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.paginationContext)
      .setFilter(PropertyFilter.eq("__key__", key))
      .setLimit(1)
      .build()

    executeQuery(query)
      .orElseFail(PaginationContextFetchError(contextId))
      .flatMap { results =>
        val list = results.asScala.toList
        if (list.nonEmpty) {
          ZIO.fromEither(entityAsPaginationContext(list.head))
        } else {
          ZIO.fail(PaginationContextNotFoundError(contextId))
        }
      }
  }

  def savePaginationContext(context: PaginationContext): IO[MarketPlayError, Unit] =
    Task(datastore.put(paginationContextAsEntity(context)))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(PaginationContextSaveError(context))
      .unit

  def cleanup(): IO[MarketPlayError, Unit] = {
    def query(threshold: Instant) = {
      Query.newEntityQueryBuilder()
        .setKind(datastoreConfig.paginationContext)
        .setFilter(PropertyFilter.le("timestamp", TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(threshold.getEpochSecond, threshold.getNano))))
        .build()
    }

    lazy val cleanupEffect = (for {
      threshold <- clock.instant.map(_.atBeginningOfDay())
      keys      <- executeQuery(query(threshold)).map(_.asScala.toList.map(_.getKey("__key__")))
      _         <- (Task(datastore.delete(keys: _*)).tapError(throwable => logger.warn(throwable.getMessage)) *> logger.info("Pagination context cursors removed")).when(keys.nonEmpty)
      _         <- logger.info("No cursors found").when(keys.isEmpty)
    } yield ()).ignore

    logger.info("Cleaning pagination context cursors...") *>
      cleanupEffect
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private def entityAsPaginationContext(entity: Entity): Either[InvalidRepresentation, PaginationContext] =
    for {
      ctxId <- tryOrLeft(entity.getKey().getName, InvalidRepresentation("Entry has no key name"))
        .flatMap(rawIdStr =>
          refined
            .refineV[ContextIdPredicate](rawIdStr)
            .left
            .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
        )
      cursor <- tryOrLeft(entity.getString("cursor"), InvalidRepresentation("Invalid cursor representation"))
        .flatMap(rawCursor =>
          refineV[CursorPredicate](rawCursor).left.map(_ =>
            InvalidRepresentation(s"Invalid format for cursor $rawCursor")
          )
        )
      hash <- tryOrLeft(entity.getLong("positionFilterHash"), InvalidRepresentation("Invalid filter hash"))
        .map(rawHash => rawHash.toInt)
    } yield PaginationContext(ctxId, cursor, hash)

  private def paginationContextAsEntity(context: PaginationContext): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.paginationContext).newKey(context.contextId.value))
      .set("cursor", context.cursor.value)
      .set("positionFilterHash", context.filterHash)
      .build()
}

object DatastorePaginationRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[DatastorePaginationRepo]] =
    (DatastorePaginationRepo(_, _, _, _)).toLayer
}


