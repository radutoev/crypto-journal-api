package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model._
import domain.position.Position.MergeResult.{NoChange, NoMerge, PositionsMerged}
import domain.position.PositionEntry.PositionEntryIdPredicate
import domain.position._
import domain.position.error._
import domain.position.model.CoinName
import util.{InstantOps, ListEitherOps, ListOptionOps, tryOrLeft}
import vo.filter
import vo.filter.{Descending, PlayFilter, SortOrder}
import vo.pagination.{CursorPredicate, Page, PaginationContext}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore.{Cursor => PaginationCursor, _}
import eu.timepit.refined
import eu.timepit.refined.types.numeric.PosInt
import eu.timepit.refined.{refineMV, refineV}
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, UIO, URLayer, ZIO}

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._

final case class DatastoreMarketPlayRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  paginationRepo: DatastorePaginationRepo,
  logger: Logger[String]
) extends MarketPlayRepo {

  override def save(address: WalletAddress, marketPlays: List[MarketPlay]): IO[MarketPlayError, Unit] = {
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

    @inline
    def previousPositions(source: List[MarketPlay], count: Int): List[MarketPlay] =
      if (count >= source.length) {
        source.takeRight(source.length)
      } else {
        source.takeRight(count)
      }

    @inline
    def nextPositions(source: List[MarketPlay], count: Int): List[MarketPlay] =
      if (count >= source.length) {
        source.take(source.length)
      } else {
        source.take(count)
      }

    val identifiablePlays: List[MarketPlay] = marketPlays.sortBy(_.openedAt)(Ordering[Instant].reverse).map(withIds)
    val playsWithLinks = identifiablePlays.zipWithIndex.map {
      case (play, idx) =>
        play match {
          case p: Position =>
            p -> (
              if (idx != 0) nextPositions(identifiablePlays.slice(0, idx), 10).map(_.id).values else List.empty,
              if (idx < identifiablePlays.length)
                previousPositions(identifiablePlays.slice(idx, identifiablePlays.length), 10).map(_.id).values
              else List.empty
            )
          case _ => play -> (List.empty, List.empty)
        }
    }

    val entities = playsWithLinks
      .map(play => marketPlayToEntity(play, address))
      .grouped(23)
      .toList

    if (marketPlays.isEmpty) {
      logger.debug(s"No positions to save for ${address.value}")
    } else {
      for {
        _ <- ZIO.foreach(entities)(saveEntities).ignore
        _ <- logger.info(s"Finished saving positions for address ${address.value}")
      } yield ()
    }
  }

  private def withIds(play: MarketPlay): MarketPlay = play match {
    case p: Position => p.copy(id = Some(PlayId.newId))
    case t: TopUp    => t.copy(id = Some(PlayId.newId))
    case w: Withdraw => w.copy(id = Some(PlayId.newId))
  }

  override def getPlays(address: WalletAddress): IO[MarketPlayError, List[MarketPlay]] =
    doFetchPlays(
      address,
      Query
        .newEntityQueryBuilder()
        .setKind(datastoreConfig.marketPlay)
        .setFilter(PropertyFilter.eq("address", address.value))
        .addOrderBy(OrderBy.asc("openedAt"))
        .build()
    )

  override def getPlays(
    address: WalletAddress,
    playFilter: PlayFilter
  ): IO[MarketPlayError, List[MarketPlay]] =
    doFetchPlays(address, positionsQuery(address, playFilter, Descending).build(), Some(playFilter.interval.start))

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
          _ => MarketPlaysFetchError(s"Market plays fetch error for address ${address.value}"),
          resultsOpt =>
            resultsOpt.fold[(Page[MarketPlays], Option[PaginationContext])](
              (Page(MarketPlays(List.empty), None), None)
            ) { results =>
              val marketPlays = MarketPlays(results.asScala.toList.map(entityToPlay).rights)
              val nextCursor  = results.getCursorAfter
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
        _ <- maybeContext.fold[IO[MarketPlayError, Unit]](UIO.unit)(ctx =>
              paginationRepo.savePaginationContext(ctx).unit
            )
      } yield page

    logger.info(s"Fetching positions for ${address.value}. Interval: ${filter.interval.start} - ${filter.interval.end}") *>
    paginationRepo
      .getPaginationContext(contextId)
      .flatMap { positionContext =>
        val query = if (filterHasChanged(positionContext.filterHash)) {
          positionsQuery(address, filter, Descending).build()
        } else {
          positionsQuery(address, filter, Descending)
            .setStartCursor(PaginationCursor.fromUrlSafe(positionContext.cursor.value))
            .build()
        }
        generatePageAndSavePaginationContext(query)
      }
      .catchSome {
        case PaginationContextNotFoundError(_) =>
          generatePageAndSavePaginationContext(positionsQuery(address, filter, Descending).build())
      }
  }

  private def positionsQuery(address: WalletAddress, positionFilter: PlayFilter, sortOrder: SortOrder) =
    Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.le("openedAt", positionFilter.interval.end.toDatastoreTimestamp())
        )
      )
      .addOrderBy(sortOrder match {
        case filter.Ascending  => OrderBy.asc("openedAt")
        case filter.Descending => OrderBy.desc("openedAt")
      })
      .setLimit(positionFilter.count)

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

  override def getPositions(playIds: List[PlayId]): IO[MarketPlayError, List[Position]] = {
    val factory = datastore.newKeyFactory().setKind(datastoreConfig.marketPlay)
    val keys    = playIds.map(playId => factory.newKey(playId.value))
    Task(datastore.get(keys.asJava, Seq.empty[ReadOption]: _*))
      .tapError(err => logger.warn(err.getMessage))
      .mapBoth(
        _ => MarketPlaysFetchError(s"Play fetch error: ${playIds.mkString(",")}"),
        _.asScala.toList.map(entityToPosition).rights
      )
  }

  private def doFetchPlays(
    address: WalletAddress,
    query: EntityQuery,
    startDateFilter: Option[Instant] = None
  ): IO[MarketPlaysFetchError, List[MarketPlay]] =
    doExecuteWrapped(query)
      .mapBoth(
        _ => MarketPlaysFetchError(s"Plays fetch error for address ${address.value}"),
        resultsOpt =>
          resultsOpt.fold[List[MarketPlay]](List.empty) { results =>
            var list = results.asScala.toList
            if (startDateFilter.isDefined) {
              list = list.filter(e => moreRecentThan(e, startDateFilter.get))
            }
            list.map(entityToPlay).rights.collect {
              case p: Position                  => p
              case t @ (_: TopUp | _: Withdraw) => t
            }
          }
      )

  private def doFetchPositions(address: WalletAddress, query: EntityQuery): IO[MarketPlaysFetchError, List[Position]] =
    doExecuteWrapped(query)
      .mapBoth(
        _ => MarketPlaysFetchError(s"Position fetch error for address: ${address.value}"),
        resultsOpt =>
          resultsOpt.fold[List[Position]](List.empty) { results =>
            //TODO Why do we have empty entries.
            results.asScala.toList.map(entityToPosition).collect { case Right(p) => p }
          }
      )

  private def doExecuteWrapped(query: EntityQuery): Task[Option[QueryResults[Entity]]] =
    executeQuery(query)
      .map(Some(_))
      .catchSome {
        case e: DatastoreException if e.getMessage.contains("no matching index found") => UIO.none
      }

  override def getPosition(playId: PlayId): IO[MarketPlayError, PositionDetails[PlayId]] = {
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
          ZIO.fromEither(entityToPositionDetails(results.next()))
        } else {
          ZIO.fail(MarketPlayNotFound(playId))
        }
      }
  }

  override def getNextPositionIds(playId: PlayId): IO[MarketPlayError, List[PlayId]] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.marketPlay).newKey(playId.value)

    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(PropertyFilter.eq("__key__", key))
      .build()

    executeQuery(query)
      .mapError(throwable => MarketPlayFetchError(playId, throwable))
      .flatMap { result =>
        val results = result.asScala
        if (results.nonEmpty) {
          val entity = results.next()
          ZIO.fromEither {
            tryOrLeft(entity.getString("nextPlayIds"), InvalidRepresentation("Entity has no nextPlayIds")).map {
              rawPlayIds =>
                if (rawPlayIds.nonEmpty) {
                  rawPlayIds.split("[,]").toList.map(PlayId.unsafeFrom)
                } else {
                  List.empty
                }
            }
          }
        } else {
          ZIO.fail(MarketPlayNotFound(playId))
        }
      }
  }

  override def getPreviousPositionIds(playId: PlayId): IO[MarketPlayError, List[PlayId]] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.marketPlay).newKey(playId.value)

    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(PropertyFilter.eq("__key__", key))
      .build()

    executeQuery(query)
      .mapError(throwable => MarketPlayFetchError(playId, throwable))
      .flatMap { result =>
        val results = result.asScala
        if (results.nonEmpty) {
          val entity = results.next()
          ZIO.fromEither {
            tryOrLeft(entity.getString("previousPlayIds"), InvalidRepresentation("Entity has no nextPlayIds")).map {
              rawPlayIds =>
                if (rawPlayIds.nonEmpty) {
                  rawPlayIds.split("[,]").toList.map(PlayId.unsafeFrom)
                } else {
                  List.empty
                }
            }
          }
        } else {
          ZIO.fail(MarketPlayNotFound(playId))
        }
      }
  }

  private def getLatestPosition(address: WalletAddress, currency: Currency): IO[MarketPlayError, PositionDetails[PlayId]] = {
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

    executeQuery(query)
      .orElseFail(MarketPlaysFetchError(s"Position fetch error for ${address.value}"))
      .flatMap { queryResult =>
        val results = queryResult.asScala
        if (results.nonEmpty) {
          ZIO.fromEither(entityToPositionDetails(results.next()))
        } else {
          ZIO.fail(NoPreviousPosition(address, currency))
        }
      }
  }

  override def merge(address: WalletAddress, plays: MarketPlays): IO[MarketPlayError, Unit] = {
    @inline
    def handleNewPosition(p: Position): IO[MarketPlayError, Unit] = {
      for {
        latest <- latestPositions(address, refineMV(10))
        toSave = marketPlayToEntity((p, (Nil, latest.map(_.id).values)), address)
        _      <- Task(datastore.put(toSave))
          .tapError(throwable => logger.warn(throwable.getMessage))
          .mapError(t => MarketPlayImportError(address, t))
      } yield ()
    }

    @inline
    def doUpdate(data: PositionDetails[PlayId]): IO[MarketPlayError, Unit] = {
      Task(datastore.update(positionToEntity(data.position, address, data.links.next, data.links.previous, datastoreConfig.marketPlay)))
        .tapError(err => logger.warn(err.getMessage))
        .unit
        .mapError(err => MarketPlayImportError(address, err))
    }

    @inline
    def updatePosition(old: PositionDetails[PlayId], toMerge: Position): IO[MarketPlayError, Unit] = {
      Position.merge(old.position, toMerge) match {
        case NoChange                     => ZIO.unit
        case PositionsMerged(newPosition) => doUpdate(old.copy(position = newPosition))
        case NoMerge                      => handleNewPosition(toMerge)
      }
    }

    ZIO.foreach_(plays.plays) {
      case p: Position =>
        if(p.currency.isDefined) {
          getLatestPosition(address, p.currency.get)
            .flatMap(latestPositionDetails => updatePosition(latestPositionDetails, p))
            .catchSome {
              case _: NoPreviousPosition => handleNewPosition(p)
            }
        } else {
          logger.warn(s"Position with no currency. Unable to merge hashes: ${p.entries.map(_.hash.value).mkString(",")}")
        }
      case t: TopUp    => save(address, List(t))
      case w: Withdraw => save(address, List(w))
    }
  }

  private def latestPositions(address: WalletAddress, count: PosInt): IO[MarketPlayError, List[Position]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.marketPlay)
      .setFilter(
        CompositeFilter.and(
          PropertyFilter.eq("address", address.value),
          PropertyFilter.eq("playType", "position")
        )
      )
      .addOrderBy(OrderBy.desc("openedAt"))
      .setLimit(count.value)
      .build()

    executeQuery(query).mapBoth(
      _ => MarketPlaysFetchError("Position fetch error"),
      results => results.asScala.toList.map(entityToPosition).rights
    )
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  private def marketPlayToEntity(data: (MarketPlay, (List[PlayId], List[PlayId])), address: WalletAddress): Entity = {
    val (marketPlay, (next, previous)) = data
    marketPlay match {
      case p: Position => positionToEntity(p, address, next, previous, datastoreConfig.marketPlay)
      case t: TopUp    => topUpToEntity(t, address, datastoreConfig.marketPlay)
      case t: Withdraw => withdrawalToEntity(t, address, datastoreConfig.marketPlay)
    }
  }

  private val positionToEntity: (Position, WalletAddress, List[PlayId], List[PlayId], String) => Entity =
    (position, address, next, previous, kind) => {
      val entries = position.entries.map { entry =>
        val key = datastore
          .newKeyFactory()
          .addAncestor(PathElement.of(kind, position.id.getOrElse(PlayId.newId).value))
          .setKind("PositionEntry")
          .newKey(UUID.randomUUID().toString)

        var builder = Entity.newBuilder(key)

        builder = entry match {
          case AirDrop(coinName, receivedFrom, _, received, coinAddress, _, _, _) =>
            builder
              .set("coinName", StringValue.of(coinName.value))
              .set("coinAddress", StringValue.of(coinAddress.value))
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("AirDrop"))

          case Approval(_, forContract, _, _, _) =>
            builder
              .set("forContract", StringValue.of(forContract.value))
              .set("type", StringValue.of("Approval"))

          case Buy(_, spent, received, receivedFrom, coinName, coinAddress, _, _, spentOriginal, _) =>
            builder
              .set("coinName", StringValue.of(coinName.value))
              .set("spent", StringValue.of(spent.amount.toString()))
              .set("spentCurrency", StringValue.of(spent.currency.value))
              .set("spentOriginal", StringValue.of(spentOriginal.map(orig => orig.amount.toString()).getOrElse("")))
              .set(
                "spentOriginalCurrency",
                StringValue.of(spentOriginal.map(orig => orig.currency.value).getOrElse(""))
              )
              .set("coinAddress", StringValue.of(coinAddress.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Buy"))

          case Claim(_, received, receivedFrom, coinName, coinAddress, _, _, _) =>
            builder
              .set("coinName", StringValue.of(coinName.value))
              .set("coinAddress", StringValue.of(coinAddress.value))
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Claim"))

          case Contribute(spent, to, _, _, _, _) =>
            builder
              .set("spent", StringValue.of(spent.amount.toString()))
              .set("spentCurrency", StringValue.of(spent.currency.value))
              .set("to", StringValue.of(to.value))
              .set("type", StringValue.of("Contribute"))

          case Sell(sold, received, _, _, _, _) =>
            builder
              .set("sold", StringValue.of(sold.amount.toString()))
              .set("soldCurrency", StringValue.of(sold.currency.value))
              .set("received", StringValue.of(received.amount.toString()))
              .set("receivedCurrency", StringValue.of(received.currency.value))
              .set("type", StringValue.of("Sell"))

          case TransferIn(value, receivedFrom, _, _, _, _, coinName, coinAddress) =>
            builder
              .set("coinName", StringValue.of(coinName.map(_.value).getOrElse("")))
              .set("coinAddress", StringValue.of(coinAddress.map(_.value).getOrElse("")))
              .set("value", StringValue.of(value.amount.toString()))
              .set("valueCurrency", StringValue.of(value.currency.value))
              .set("receivedFrom", StringValue.of(receivedFrom.value))
              .set("type", StringValue.of("TransferIn"))

          case TransferOut(amount, to, _, _, _, _) =>
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
        .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(position.id.getOrElse(PlayId.newId).value))
        .set("address", StringValue.of(address.value))
        .set("currency", StringValue.of(position.currency.map(_.value).getOrElse("")))
        .set("state", StringValue.of(position.state.toString))
        .set(
          "openedAt",
          TimestampValue
            .of(Timestamp.ofTimeSecondsAndNanos(position.openedAt.getEpochSecond, position.openedAt.getNano))
        )
        .set("nextPlayIds", next.map(_.value).mkString(","))
        .set("previousPlayIds", previous.map(_.value).mkString(","))
        .set("entries", ListValue.newBuilder().set(entries.asJava).build())
        .set("playType", "position")

      if (position.closedAt.isDefined) {
        val closedAt = position.closedAt.get
        builder = builder.set(
          "closedAt",
          TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(closedAt.getEpochSecond, closedAt.getNano))
        )
      }

      builder.build()
    }

  /**
   * Converts a topUp to an Entity.
   * I map the timestamp to openedAt, so that I could later on use it as a consistent filter attribute for the read operations.
   */
  private def topUpToEntity(topUp: TopUp, address: WalletAddress, kind: String): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(topUp.id.getOrElse(PlayId.newId).value))
      .set("address", address.value)
      .set("hash", topUp.txHash.value)
      .set("value", DoubleValue.of(topUp.value.amount.doubleValue))
      .set("valueCurrency", StringValue.of(topUp.value.currency.value))
      .set("fee", DoubleValue.of(topUp.fee.amount.doubleValue))
      .set("feeCurrency", StringValue.of(topUp.fee.currency.value))
      .set(
        "openedAt",
        TimestampValue
          .of(Timestamp.ofTimeSecondsAndNanos(topUp.timestamp.getEpochSecond, topUp.timestamp.getNano))
      )
      .set("playType", "topUp")
      .build()

  private def withdrawalToEntity(withdrawal: Withdraw, address: WalletAddress, kind: String): Entity =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(withdrawal.id.getOrElse(PlayId.newId).value))
      .set("address", address.value)
      .set("hash", withdrawal.txHash.value)
      .set("value", DoubleValue.of(withdrawal.value.amount.doubleValue))
      .set("valueCurrency", StringValue.of(withdrawal.value.currency.value))
      .set("fee", DoubleValue.of(withdrawal.fee.amount.doubleValue))
      .set("feeCurrency", StringValue.of(withdrawal.fee.currency.value))
      .set(
        "openedAt",
        TimestampValue
          .of(Timestamp.ofTimeSecondsAndNanos(withdrawal.timestamp.getEpochSecond, withdrawal.timestamp.getNano))
      )
      .set("playType", "withdrawal")
      .build()

  private def entityToPlay(e: Entity): Either[InvalidRepresentation, MarketPlay] =
    e.getString("playType").strip() match {
      case "position"   => entityToPosition(e)
      case "topUp"      => entityToTopUp(e)
      case "withdrawal" => entityToWithdrawal(e)
    }

  private def entityToPositionDetails(entity: Entity): Either[InvalidRepresentation, PositionDetails[PlayId]] = {
    for {
      position <- entityToPosition(entity)
      next <- tryOrLeft(
        entity.getString("nextPlayIds"),
        InvalidRepresentation("Entity has no nextPlayIds")
      ).map { rawPlayIds =>
        if (rawPlayIds.nonEmpty) {
          rawPlayIds.split("[,]").toList.map(PlayId.unsafeFrom)
        } else {
          List.empty
        }
      }
      previous <- tryOrLeft(
        entity.getString("previousPlayIds"),
        InvalidRepresentation("Entity has no previousPlayIds")
      ).map { rawPlayIds =>
        if (rawPlayIds.nonEmpty) {
          rawPlayIds.split("[,]").toList.map(PlayId.unsafeFrom)
        } else {
          List.empty
        }
      }
    } yield PositionDetails(position, PositionLinks(previous, next))
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
    } yield Position(entries, id)
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
                coinName <- tryOrLeft(
                             entity.getString("coinName"),
                             InvalidRepresentation("Invalid receivedFrom representation")
                           ).map(rawReceivedFrom => CoinName.unsafeApply(rawReceivedFrom))
                coinAddress <- tryOrLeft(
                                entity.getString("receivedFrom"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => CoinAddress.unsafeFrom(rawReceivedFrom))
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
                coinName,
                receivedFrom,
                fee,
                received = FungibleData(received, receivedCurrency),
                coinAddress,
                hash,
                timestamp,
                Some(id)
              )

            case "Approval" =>
              for {
                forContract <- tryOrLeft(
                                entity.getString("forContract"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
              } yield Approval(FungibleData(feeAmount, feeCurrency), forContract, hash, timestamp, Some(id))

            case "Buy" =>
              for {
                spent <- tryOrLeft(
                          entity.getString("spent"),
                          InvalidRepresentation("Invalid received representation")
                        ).map(BigDecimal(_))
                spentCurrency <- tryOrLeft(
                                  entity.getString("spentCurrency"),
                                  InvalidRepresentation("Invalid spentCurrency representation")
                                ).map(Currency.unsafeFrom)
                spentOriginalValue <- tryOrLeft(
                                       entity.getString("spentOriginal"),
                                       InvalidRepresentation("Invalid spentOriginal representation")
                                     )
                spentOriginalCurrency <- tryOrLeft(
                                          entity.getString("spentOriginalCurrency"),
                                          InvalidRepresentation("Invalid spentCurrency representation")
                                        )
                spentOriginal = if (spentOriginalValue.nonEmpty) {
                  Some(FungibleData(BigDecimal(spentOriginalValue), Currency.unsafeFrom(spentOriginalCurrency)))
                } else {
                  None
                }
                received <- tryOrLeft(
                             entity.getString("received"),
                             InvalidRepresentation("Invalid received representation")
                           ).map(BigDecimal(_))
                receivedFrom <- tryOrLeft(
                                 entity.getString("receivedFrom"),
                                 InvalidRepresentation("Invalid receivedFrom representation")
                               ).map(rawReceivedFrom => WalletAddress.unsafeFrom(rawReceivedFrom))
                receivedCurrency <- tryOrLeft(
                                     entity.getString("receivedCurrency"),
                                     InvalidRepresentation("Invalid receivedCurrency representation")
                                   ).map(Currency.unsafeFrom)
                coinName <- tryOrLeft(
                             entity.getString("coinName"),
                             InvalidRepresentation("Invalid receivedFrom representation")
                           ).map(rawReceivedFrom => CoinName.unsafeApply(rawReceivedFrom))
                coinAddress <- tryOrLeft(
                                entity.getString("coinAddress"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => CoinAddress.unsafeFrom(rawReceivedFrom))
              } yield Buy(
                fee,
                FungibleData(spent, spentCurrency),
                FungibleData(received, receivedCurrency),
                receivedFrom,
                coinName,
                coinAddress,
                hash,
                timestamp,
                spentOriginal,
                Some(id)
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
                coinName <- tryOrLeft(
                             entity.getString("coinName"),
                             InvalidRepresentation("Invalid receivedFrom representation")
                           ).map(rawReceivedFrom => CoinName.unsafeApply(rawReceivedFrom))
                coinAddress <- tryOrLeft(
                                entity.getString("coinAddress"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom => CoinAddress.unsafeFrom(rawReceivedFrom))
              } yield Claim(
                fee,
                FungibleData(received, receivedCurrency),
                receivedFrom,
                coinName,
                coinAddress,
                hash,
                timestamp,
                Some(id)
              )

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
              } yield Contribute(FungibleData(spent, spentCurrency), to, fee, hash, timestamp, Some(id))

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
                timestamp,
                Some(id)
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
                coinName <- tryOrLeft(
                             entity.getString("coinName"),
                             InvalidRepresentation("Invalid receivedFrom representation")
                           ).map(rawReceivedFrom =>
                             if (rawReceivedFrom.isEmpty) None else Some(CoinName.unsafeApply(rawReceivedFrom))
                           )
                coinAddress <- tryOrLeft(
                                entity.getString("coinAddress"),
                                InvalidRepresentation("Invalid receivedFrom representation")
                              ).map(rawReceivedFrom =>
                                if (rawReceivedFrom.isEmpty) None else Some(CoinAddress.unsafeFrom(rawReceivedFrom))
                              )
              } yield TransferIn(
                FungibleData(value, valueCurrency),
                receivedFrom,
                fee,
                hash,
                timestamp,
                coinName,
                coinAddress,
                Some(id)
              )

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
              } yield TransferOut(FungibleData(amount, amountCurrency), to, fee, hash, timestamp, Some(id))

            case _ => Left(InvalidRepresentation(s"Invalid PositionEntry type $posType"))
          }
      }
    }
  }

  private def entityToTopUp(entity: Entity): Either[InvalidRepresentation, TopUp] =
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
      id
    )

  private def entityToWithdrawal(entity: Entity): Either[InvalidRepresentation, Withdraw] =
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
    } yield Withdraw(
      hash,
      FungibleData(value, currency),
      FungibleData(feeValue, feeCurrency),
      timestamp,
      id
    )

  private def moreRecentThan(entity: Entity, timestamp: Instant): Boolean =
    tryOrLeft(
      Instant.ofEpochSecond(entity.getTimestamp("openedAt").getSeconds),
      InvalidRepresentation("Invalid openedAt representation")
    ).fold(_ => false, openedAt => openedAt.isAfter(timestamp))
}

object DatastoreMarketPlayRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Has[DatastorePaginationRepo] with Logging, Has[
    MarketPlayRepo
  ]] =
    (DatastoreMarketPlayRepo(_, _, _, _)).toLayer
}
