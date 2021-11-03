package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model._
import domain.position.error._
import domain.position.{
  AirDrop,
  Approval,
  Buy,
  Claim,
  Contribute,
  MarketPlay,
  MarketPlayRepo,
  MarketPlays,
  Position,
  PositionEntry,
  Sell,
  TopUp,
  TransferIn,
  TransferOut,
  TransferOutPlay
}
import util.{ tryOrLeft, InstantOps, ListEitherOps }
import vo.filter.PlayFilter
import vo.pagination.{ CursorPredicate, Page, PaginationContext }

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore.{ Cursor => PaginationCursor, _ }
import eu.timepit.refined
import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.domain.position.PositionEntry.PositionEntryIdPredicate
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastoreMarketPlayRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  logger: Logger[String]
) extends MarketPlayRepo {

  override def save(address: WalletAddress, marketPlays: List[MarketPlay]): Task[Unit] = {
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
          } *> logger.info(s"Imported ${list.size} positions for ${address.value}")
        }
        .tapError(throwable =>
          logger.error(s"Error saving positions for ${address.value}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle transactions response when doing error handling.

    val entities = marketPlays.map(play => marketPlayToEntity(play, address)).grouped(23).toList

    if (marketPlays.isEmpty) {
      logger.debug(s"No positions to save for ${address.value}")
    } else {
      for {
        _ <- ZIO.foreach(entities)(saveEntities).ignore
        _ <- logger.info(s"Finished saving positions for address ${address.value}")
      } yield ()
    }
  }

  override def getPlays(
    address: WalletAddress,
    playFilter: PlayFilter
  ): IO[MarketPlayError, List[MarketPlay]] =
    doFetchPlays(address, positionsQuery(address, playFilter).build(), Some(playFilter.interval.start))

  override def getPlays(
    address: WalletAddress,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, Page[MarketPlays]] = {
    @inline
    def filterHasChanged(existentFilterHash: Int): Boolean =
      existentFilterHash != filter.hashCode()

    @inline
    def generatePage(query: EntityQuery): IO[MarketPlayError, (Page[MarketPlays], Option[PaginationContext])] =
      executeQuery(query)
        .map(Some(_))
        .mapBoth(
          _ => MarketPlaysFetchError(address),
          resultsOpt =>
            resultsOpt.fold[(Page[MarketPlays], Option[PaginationContext])](
              (Page(MarketPlays(List.empty), None), None)
            ) { results =>
              val marketPlays = MarketPlays(results.asScala.toList.map(entityToPlay).collect {
                case Right(play) =>
                  play match {
                    //TODO Why can entries be empty??
                    case p: Position if p.entries.nonEmpty   => p
                    case t @ (_: TopUp | _: TransferOutPlay) => t
                  }
              })
              val nextCursor = results.getCursorAfter
              val paginationContext = if (nextCursor.toUrlSafe.nonEmpty) {
                Some(
                  PaginationContext(
                    contextId,
                    refineV[CursorPredicate].unsafeFrom(nextCursor.toUrlSafe),
                    filter.hashCode()
                  )
                )
              } else None
              (Page(marketPlays, Some(contextId)), paginationContext)
            }
        )

    @inline
    def generatePageAndSavePaginationContext(query: EntityQuery): IO[MarketPlayError, Page[MarketPlays]] =
      for {
        (page, maybeContext) <- generatePage(query)
        _                    <- maybeContext.fold[IO[MarketPlayError, Unit]](UIO.unit)(ctx => savePaginationContext(ctx).unit)
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
      generatePageAndSavePaginationContext(query)
    }.catchSome {
      case PaginationContextNotFoundError(_) =>
        generatePageAndSavePaginationContext(positionsQuery(address, filter).build())
    }
  }

  private def positionsQuery(address: WalletAddress, positionFilter: PlayFilter) =
    Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.le("openedAt", positionFilter.interval.end.toDatastoreTimestamp())
        )
      )
      .addOrderBy(OrderBy.desc("openedAt"))
      .setLimit(positionFilter.count)

  private def getPaginationContext(contextId: ContextId): IO[MarketPlayError, PaginationContext] = {
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

  private def savePaginationContext(context: PaginationContext): IO[MarketPlayError, Unit] =
    Task(datastore.put(paginationContextAsEntity(context)))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(PaginationContextSaveError(context))
      .unit

  override def getPositions(address: WalletAddress, state: State): IO[MarketPlayError, List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.eq("state", state.toString),
          PropertyFilter.eq("playType", "position")
        )
      )
      .addOrderBy(OrderBy.asc("openedAt"))
      .build()
    doFetchPositions(address, query)
  }

  private def doFetchPlays(
    address: WalletAddress,
    query: EntityQuery,
    startDateFilter: Option[Instant] = None
  ): IO[MarketPlaysFetchError, List[MarketPlay]] =
    doExecuteWrapped(query)
      .mapBoth(
        _ => MarketPlaysFetchError(address),
        resultsOpt =>
          resultsOpt.fold[List[MarketPlay]](List.empty) { results =>
            var list = results.asScala.toList
            if (startDateFilter.isDefined) {
              list = list.filter(e => moreRecentThan(e, startDateFilter.get))
            }
            list.map(entityToPlay).rights.collect {
              case p: Position if p.entries.nonEmpty   => p
              case t @ (_: TopUp | _: TransferOutPlay) => t
            }
          }
      )

  private def doFetchPositions(address: WalletAddress, query: EntityQuery): IO[MarketPlaysFetchError, List[Position]] =
    doExecuteWrapped(query)
      .mapBoth(
        _ => MarketPlaysFetchError(address),
        resultsOpt =>
          resultsOpt.fold[List[Position]](List.empty) { results =>
            //TODO Why do we have empty entries.
            results.asScala.toList.map(entityToPosition).collect { case Right(p) if p.entries.nonEmpty => p }
          }
      )

  private def doExecuteWrapped(query: EntityQuery): Task[Option[QueryResults[Entity]]] =
    executeQuery(query)
      .map(Some(_))
      .catchSome {
        case e: DatastoreException if e.getMessage.contains("no matching index found") => UIO.none
      }

  override def getPosition(playId: PlayId): IO[MarketPlayError, Position] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.marketPlay).newKey(playId.value)
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(PropertyFilter.eq("__key__", key))
      .build()

    executeQuery(query)
      .mapError(throwable => MarketPlayFetchError(playId, throwable))
      .flatMap { queryResult =>
        val results = queryResult.asScala
        if (results.nonEmpty) {
          ZIO
            .fromEither(entityToPlay(results.next()))
            .collect(MarketPlayNotFound(playId)) {
              case p: Position => p
            }
        } else {
          ZIO.fail(MarketPlayNotFound(playId))
        }
      }
  }

  override def getLatestPosition(address: WalletAddress, currency: Currency): IO[MarketPlayError, Option[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
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
      throwable => MarketPlaysFetchError(address),
      results => results.asScala.toList.map(entityToPosition).collectFirst { case Right(value) => value }
    )
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private def marketPlayToEntity(marketPlay: MarketPlay, address: WalletAddress): Entity =
    marketPlay match {
      case p: Position        => positionToEntity(p, address, datastoreConfig.marketPlay)
      case t: TopUp           => transferInToEntity(t, address, datastoreConfig.marketPlay)
      case t: TransferOutPlay => transferOutToEntity(t, address, datastoreConfig.marketPlay)
    }

  private val positionToEntity: (Position, WalletAddress, String) => Entity =
    (position, address, kind) => {
      val id = UUID.randomUUID().toString
      val entries = position.entries.map { entry =>
        val key = datastore
          .newKeyFactory()
          .addAncestor(PathElement.of(kind, id))
          .setKind("PositionEntry")
          .newKey(UUID.randomUUID().toString)

        var builder = Entity.newBuilder(key)

        builder = entry match {
          case AirDrop(receivedFrom, _, received, _, _) =>
            builder
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("AirDrop"))

          case Approval(_, forContract, _, _) =>
            builder
              .set("forContract", StringValue.of(forContract.value))
              .set("type", StringValue.of("Approval"))

          case Buy(_, spent, received, coinAddress, _, _) =>
            builder
              .set("spent", StringValue.of(spent.amount.toString()))
              .set("spentCurrency", StringValue.of(spent.currency.value))
              .set("coinAddress", StringValue.of(coinAddress.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Buy"))

          case Claim(_, received, receivedFrom, _, _) =>
            builder
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Claim"))

          case Contribute(spent, to, _, _, _) =>
            builder
              .set("spent", StringValue.of(spent.amount.toString()))
              .set("spentCurrency", StringValue.of(spent.currency.value))
              .set("to", StringValue.of(to.value))
              .set("type", StringValue.of("Contribute"))

          case Sell(sold, received, _, _, _) =>
            builder
              .set("sold", StringValue.of(sold.amount.toString()))
              .set("soldCurrency", StringValue.of(sold.currency.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Sell"))

          case TransferIn(value, receivedFrom, _, _, _) =>
            builder
              .set("value", StringValue.of(value.amount.toString()))
              .set("valueCurrency", StringValue.of(value.currency.value))
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("type", StringValue.of("TransferIn"))

          case TransferOut(amount, to, _, _, _) =>
            builder
              .set("amount", StringValue.of(amount.amount.toString()))
              .set("amountCurrency", StringValue.of(amount.currency.value))
              .set("to", StringValue.of(to.value))
              .set("type", StringValue.of("TransferOut"))
        }

        EntityValue.of(
          builder
            .set("fee", StringValue.of(entry.fee.amount.toString()))
            .set("feeCurrency", StringValue.of(entry.fee.currency.toString()))
            .set("hash", StringValue.of(entry.hash.value))
            .set(
              "timestamp",
              TimestampValue
                .of(Timestamp.ofTimeSecondsAndNanos(entry.timestamp.getEpochSecond, entry.timestamp.getNano))
            )
            .build()
        )
      }

      var builder = Entity
        .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(id))
        .set("address", StringValue.of(address.value))
        .set("currency", StringValue.of(position.currency.map(_.value).getOrElse("")))
        .set("state", StringValue.of(position.state.toString))
        .set(
          "openedAt",
          TimestampValue
            .of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano))
        )
        .set("entries", ListValue.newBuilder().set(entries.asJava).build())
        .set("playType", "position")

      if (position.closedAt().isDefined) {
        val closedAt = position.closedAt().get
        builder = builder.set(
          "closedAt",
          TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(closedAt.getEpochSecond, closedAt.getNano))
        )
      }

      builder.build()
    }

  /**
   * Converts a TransferIn to an Entity.
   * I map the timestamp to openedAt, so that I could later on use it as a consistent filter attribute for the read operations.
   */
  private def transferInToEntity(transferIn: TopUp, address: WalletAddress, kind: String): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
      .set("address", address.value)
      .set("hash", transferIn.txHash.value)
      .set("value", DoubleValue.of(transferIn.value.amount.doubleValue))
      .set("valueCurrency", StringValue.of(transferIn.value.currency.value))
      .set("fee", DoubleValue.of(transferIn.fee.amount.doubleValue))
      .set("feeCurrency", StringValue.of(transferIn.fee.currency.value))
      .set(
        "openedAt",
        TimestampValue
          .of(Timestamp.ofTimeSecondsAndNanos(transferIn.timestamp.getEpochSecond, transferIn.timestamp.getNano))
      )
      .set("playType", "transferIn")
      .build()

  private def transferOutToEntity(transferOut: TransferOutPlay, address: WalletAddress, kind: String): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(UUID.randomUUID().toString))
      .set("address", address.value)
      .set("hash", transferOut.txHash.value)
      .set("value", DoubleValue.of(transferOut.value.amount.doubleValue))
      .set("valueCurrency", StringValue.of(transferOut.value.currency.value))
      .set("fee", DoubleValue.of(transferOut.fee.amount.doubleValue))
      .set("feeCurrency", StringValue.of(transferOut.fee.currency.value))
      .set(
        "openedAt",
        TimestampValue
          .of(Timestamp.ofTimeSecondsAndNanos(transferOut.timestamp.getEpochSecond, transferOut.timestamp.getNano))
      )
      .set("playType", "transferOut")
      .build()

  private def entityToPlay(e: Entity): Either[InvalidRepresentation, MarketPlay] =
    e.getString("playType").strip() match {
      case "position"    => entityToPosition(e)
      case "transferIn"  => entityToTransferIn(e)
      case "transferOut" => entityToTransferOut(e)
    }

  private val entityToPosition: Entity => Either[InvalidRepresentation, Position] = entity => {
    for {
      id <- tryOrLeft(entity.getKey.getName, InvalidRepresentation("Entity has no key name"))
             .flatMap(rawIdStr =>
               refined
                 .refineV[PlayIdPredicate](rawIdStr)
                 .left
                 .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
             )
      entries <- tryOrLeft(
                  entity
                    .getList[EntityValue]("entries")
                    .asScala
                    .toList
                    .map(entryToPositionEntry)
                    .rights,
                  InvalidRepresentation("Invalid entries representation")
                )
    } yield Position(entries, id = Some(id))
  }

  private val entryToPositionEntry: EntityValue => Either[InvalidRepresentation, PositionEntry] = e => {
    val entity = e.get()

    tryOrLeft(entity.getString("type"), InvalidRepresentation("PositionEntry type is missing")).flatMap { posType =>
      val commonData = for {
        id <- tryOrLeft(entity.getKey().asInstanceOf[Key].getName, InvalidRepresentation("Entry has no key name"))
               .flatMap(rawIdStr =>
                 refined
                   .refineV[PositionEntryIdPredicate](rawIdStr)
                   .left
                   .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
               )
        feeAmount <- tryOrLeft(entity.getString("fee"), InvalidRepresentation("Invalid fee representation"))
                      .map(BigDecimal(_))
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
      } yield (id, feeAmount, feeCurrency, timestamp, hash)

      commonData.flatMap {
        case (id, feeAmount, feeCurrency, timestamp, hash) =>
          val fee = FungibleData(feeAmount, feeCurrency)
          posType match {
            case "AirDrop" =>
              for {
                receivedFrom <- tryOrLeft(
                                 entity.getString("receivedFrom"),
                                 InvalidRepresentation("Invalid receivedFrom representation")
                               ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
                received <- tryOrLeft(
                             entity.getString("received"),
                             InvalidRepresentation("Invalid received representation")
                           ).map(BigDecimal(_))
                receivedCurrency <- tryOrLeft(
                                     entity.getString("receivedCurrency"),
                                     InvalidRepresentation("Invalid receivedCurrency representation")
                                   ).map(Currency.unsafeFrom)
              } yield AirDrop(
                receivedFrom,
                fee,
                received = FungibleData(received, receivedCurrency),
                hash,
                timestamp
              )

            case "Approval" =>
              for {
                forContract <- tryOrLeft(
                                entity.getString("forContract"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
              } yield Approval(FungibleData(feeAmount, feeCurrency), forContract, hash, timestamp)

            case "Buy" =>
              for {
                spent <- tryOrLeft(
                          entity.getString("spent"),
                          InvalidRepresentation("Invalid received representation")
                        ).map(BigDecimal(_))
                spentCurrency <- tryOrLeft(
                                  entity.getString("spentCurrency"),
                                  InvalidRepresentation("Invalid receivedCurrency representation")
                                ).map(Currency.unsafeFrom)
                received <- tryOrLeft(
                             entity.getString("received"),
                             InvalidRepresentation("Invalid received representation")
                           ).map(BigDecimal(_))
                receivedCurrency <- tryOrLeft(
                                     entity.getString("receivedCurrency"),
                                     InvalidRepresentation("Invalid receivedCurrency representation")
                                   ).map(Currency.unsafeFrom)
                coinAddress <- tryOrLeft(
                                entity.getString("coinAddress"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
              } yield Buy(
                fee,
                FungibleData(spent, spentCurrency),
                FungibleData(received, receivedCurrency),
                coinAddress,
                hash,
                timestamp
              )

            case "Claim" =>
              for {
                received <- tryOrLeft(
                             entity.getString("received"),
                             InvalidRepresentation("Invalid received representation")
                           ).map(BigDecimal(_))
                receivedCurrency <- tryOrLeft(
                                     entity.getString("receivedCurrency"),
                                     InvalidRepresentation("Invalid receivedCurrency representation")
                                   ).map(Currency.unsafeFrom)
                receivedFrom <- tryOrLeft(
                                 entity.getString("receivedFrom"),
                                 InvalidRepresentation("Invalid receivedFrom representation")
                               ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
              } yield Claim(fee, FungibleData(received, receivedCurrency), receivedFrom, hash, timestamp)

            case "Contribute" =>
              for {
                spent <- tryOrLeft(
                          entity.getString("spent"),
                          InvalidRepresentation("Invalid received representation")
                        ).map(BigDecimal(_))
                spentCurrency <- tryOrLeft(
                                  entity.getString("spentCurrency"),
                                  InvalidRepresentation("Invalid spentCurrency representation")
                                ).map(Currency.unsafeFrom)
                to <- tryOrLeft(
                       entity.getString("to"),
                       InvalidRepresentation("Invalid to representation")
                     ).map(rawTo => WalletAddress.unsafeFrom(rawTo))
              } yield Contribute(FungibleData(spent, spentCurrency), to, fee, hash, timestamp)

            case "Sell" =>
              for {
                sold <- tryOrLeft(
                         entity.getString("sold"),
                         InvalidRepresentation("Invalid received representation")
                       ).map(BigDecimal(_))
                soldCurrency <- tryOrLeft(
                                 entity.getString("soldCurrency"),
                                 InvalidRepresentation("Invalid receivedCurrency representation")
                               ).map(Currency.unsafeFrom)
                received <- tryOrLeft(
                             entity.getString("received"),
                             InvalidRepresentation("Invalid received representation")
                           ).map(BigDecimal(_))
                receivedCurrency <- tryOrLeft(
                                     entity.getString("receivedCurrency"),
                                     InvalidRepresentation("Invalid receivedCurrency representation")
                                   ).map(Currency.unsafeFrom)
              } yield Sell(
                FungibleData(sold, soldCurrency),
                FungibleData(received, receivedCurrency),
                fee,
                hash,
                timestamp
              )

            case "TransferIn" =>
              for {
                value <- tryOrLeft(
                         entity.getString("value"),
                         InvalidRepresentation("Invalid sold representation")
                       ).map(BigDecimal(_))
                valueCurrency <- tryOrLeft(
                                 entity.getString("valueCurrency"),
                                 InvalidRepresentation("Invalid soldCurrency representation")
                               ).map(Currency.unsafeFrom)
                receivedFrom <- tryOrLeft(
                                 entity.getString("receivedFrom"),
                                 InvalidRepresentation("Invalid receivedFrom representation")
                               ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
              } yield TransferIn(FungibleData(value, valueCurrency), receivedFrom, fee, hash, timestamp)

            case "TransferOut" =>
              for {
                amount <- tryOrLeft(
                           entity.getString("amount"),
                           InvalidRepresentation("Invalid amount representation")
                         ).map(BigDecimal(_))
                amountCurrency <- tryOrLeft(
                                   entity.getString("amountCurrency"),
                                   InvalidRepresentation("Invalid soldCurrency representation")
                                 ).map(Currency.unsafeFrom)
                to <- tryOrLeft(
                       entity.getString("to"),
                       InvalidRepresentation("Invalid to representation")
                     ).map(rawTo => WalletAddress.unsafeFrom(rawTo))
              } yield TransferOut(FungibleData(amount, amountCurrency), to, fee, hash, timestamp)

            case _ => Left(InvalidRepresentation(s"Invalid PositionEntry type $posType"))
          }
      }
    }
  }

  private def entityToTransferIn(entity: Entity): Either[InvalidRepresentation, TopUp] =
    for {
      id <- tryOrLeft(entity.getKey.getName, InvalidRepresentation("Entity has no key name"))
             .flatMap(rawIdStr =>
               refined
                 .refineV[PlayIdPredicate](rawIdStr)
                 .left
                 .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
             )
      hash <- tryOrLeft(entity.getString("hash"), InvalidRepresentation("Invalid hash representation"))
               .flatMap(value => TransactionHash(value).left.map(InvalidRepresentation))
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
                    Instant.ofEpochSecond(entity.getTimestamp("openedAt").getSeconds),
                    InvalidRepresentation("Invalid timestamp representation")
                  )
    } yield TopUp(
      hash,
      FungibleData(value, currency),
      FungibleData(feeValue, feeCurrency),
      timestamp,
      id = Some(id)
    )

  private def entityToTransferOut(entity: Entity): Either[InvalidRepresentation, TransferOutPlay] =
    for {
      id <- tryOrLeft(entity.getKey.getName, InvalidRepresentation("Entity has no key name"))
             .flatMap(rawIdStr =>
               refined
                 .refineV[PlayIdPredicate](rawIdStr)
                 .left
                 .map(_ => InvalidRepresentation(s"Invalid format for id $rawIdStr"))
             )
      hash <- tryOrLeft(entity.getString("hash"), InvalidRepresentation("Invalid hash representation"))
               .flatMap(value => TransactionHash(value).left.map(InvalidRepresentation))
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
                    Instant.ofEpochSecond(entity.getTimestamp("openedAt").getSeconds),
                    InvalidRepresentation("Invalid timestamp representation")
                  )
    } yield TransferOutPlay(
      hash,
      FungibleData(value, currency),
      FungibleData(feeValue, feeCurrency),
      timestamp,
      id = Some(id)
    )

  private def paginationContextAsEntity(context: PaginationContext): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.paginationContext).newKey(context.contextId.value))
      .set("cursor", context.cursor.value)
      .set("positionFilterHash", context.filterHash)
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
               .map(rawHash => rawHash.toInt)
    } yield PaginationContext(ctxId, cursor, hash)

  private def moreRecentThan(entity: Entity, timestamp: Instant): Boolean =
    tryOrLeft(
      Instant.ofEpochSecond(entity.getTimestamp("openedAt").getSeconds),
      InvalidRepresentation("Invalid openedAt representation")
    ).fold(_ => false, openedAt => openedAt.isAfter(timestamp))
}

object DatastoreMarketPlayRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging with Clock, Has[MarketPlayRepo]] =
    (DatastoreMarketPlayRepo(_, _, _)).toLayer
}
