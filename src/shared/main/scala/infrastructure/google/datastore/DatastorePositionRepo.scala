package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model._
import domain.position.Position.{PositionEntryIdPredicate, PositionId, PositionIdPredicate}
import domain.position.error._
import domain.position.{Position, PositionEntry, PositionRepo, Positions}
import util.{InstantOps, tryOrLeft}
import vo.filter.PositionFilter
import vo.pagination.{CursorPredicate, Page, PaginationContext}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore.{Cursor => PaginationCursor, _}
import eu.timepit.refined
import eu.timepit.refined.numeric.Positive
import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosLong
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, UIO, URLayer, ZIO}

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
    address: WalletAddress,
    positionFilter: PositionFilter
  ): IO[PositionError, List[Position]] = {
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

  override def getPositions(
    address: WalletAddress,
    filter: PositionFilter,
    contextId: ContextId
  ): IO[PositionError, Page[Positions]] = {
    @inline
    def filterHasChanged(existentFilterHash: PosLong): Boolean =
      existentFilterHash.value != filter.hashCode()

    @inline
    def generatePage(query: EntityQuery): IO[PositionError, (Page[Positions], Option[PaginationContext])] =
      executeQuery(query)
        .map(Some(_))
        .mapBoth(
          _ => PositionsFetchError(address),
          resultsOpt =>
            resultsOpt.fold[(Page[Positions], Option[PaginationContext])](
              (Page(Positions(List.empty), None), None)
            ) { results =>
              val nextCursor = results.getCursorAfter
              val paginationContext = if (nextCursor.toUrlSafe.nonEmpty) {
                Some(
                  PaginationContext(
                    contextId,
                    refineV[CursorPredicate].unsafeFrom(nextCursor.toUrlSafe),
                    PosLong.unsafeFrom(filter.hashCode())
                  )
                )
              } else None
              val positions = Positions(results.asScala.toList.map(entityToPosition).collect {
                case Right(position) if position.entries.nonEmpty => position
              })
              (Page(positions, Some(contextId)), paginationContext)
            }
        )

    @inline
    def generatePageAndSavePaginationContext(query: EntityQuery): IO[PositionError, Page[Positions]] =
      for {
        (page, maybeContext) <- generatePage(query)
        _                    <- maybeContext.fold[IO[PositionError, Unit]](UIO.unit)(ctx => savePaginationContext(ctx).unit)
      } yield page

    logger.info(s"Fetching positions for ${address.value}. Interval: ${filter.interval.start} - ${filter.interval.end}") *>
    getPaginationContext(contextId).flatMap { positionContext =>
      val query = if (filterHasChanged(positionContext.filterHash)) {
        positionsQuery(address, filter).build()
      } else {
        positionsQuery(address, filter)
          .setStartCursor(PaginationCursor.fromUrlSafe(positionContext.cursor.value))
          .build()
      }
      logger.info("Using pagination cursor") *> generatePageAndSavePaginationContext(query)
    }.catchSome {
      case PaginationContextNotFoundError(_) =>
        generatePageAndSavePaginationContext(positionsQuery(address, filter).build())
    }
  }

  private def positionsQuery(address: WalletAddress, positionFilter: PositionFilter) =
    Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.le("openedAt", positionFilter.interval.end.toDatastoreTimestamp())
        )
      )
      .addOrderBy(OrderBy.desc("openedAt"))
      .setLimit(positionFilter.count)

  private def getPaginationContext(contextId: ContextId): IO[PositionError, PaginationContext] = {
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

  private def savePaginationContext(context: PaginationContext): IO[PositionError, Unit] =
    Task(datastore.put(paginationContextAsEntity(context)))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(PaginationContextSaveError(context))
      .unit

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
            results.asScala.toList.map(entityToPosition).collect { case Right(position) if position.entries.nonEmpty => position }
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

  override def getLatestPosition(address: WalletAddress, currency: Currency): IO[PositionError, Option[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.position)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.eq("currency", currency.value)
        )
      )
      .addOrderBy(OrderBy.desc("openedAt"))
      .setLimit(1)
      .build()

    executeQuery(query).mapBoth(
      throwable => PositionsFetchError(address),
      results => results.asScala.toList.map(entityToPosition).collectFirst { case Right(value) => value }
    )
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private val positionToEntity: (Position, WalletAddress, String) => Entity =
    (position, address, kind) => {
      val id = UUID.randomUUID().toString
      val entries = position.entries.map { entry =>
        EntityValue.of(
          Entity
            .newBuilder(datastore.newKeyFactory().addAncestor(PathElement.of(kind, id)).setKind("PositionEntry").newKey(UUID.randomUUID().toString))
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
        .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(id))
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

  private def paginationContextAsEntity(context: PaginationContext): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.position).newKey(context.contextId.value))
      .set("cursor", context.cursor.value)
      .set("positionFilterHash", context.filterHash.value)
      .build()

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
               .map(rawHash => refineV[Positive].unsafeFrom(rawHash))
    } yield PaginationContext(ctxId, cursor, hash)
}

object DatastorePositionRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging with Clock, Has[PositionRepo]] =
    (DatastorePositionRepo(_, _, _)).toLayer
}
