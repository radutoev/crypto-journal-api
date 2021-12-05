package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.error._
import domain.blockchain.{BlockchainRepo, Transaction}
import domain.currency.CurrencyRepo
import domain.model._
import domain.position.MarketPlays.findMarketPlays
import domain.position.error._
import domain.pricequote.error.PriceQuoteError
import domain.pricequote.{PriceQuoteRepo, PriceQuotes}
import domain.wallet.Wallet
import util.ListOptionOps
import vo.TimeInterval
import vo.filter.PlayFilter

import zio.cache.{Cache, Lookup}
import zio.duration.durationInt
import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Has, IO, UIO, URLayer, ZIO, ZLayer}

import java.time.Instant

trait MarketPlayService {
  def getPlays(userWallet: Wallet): IO[MarketPlayError, MarketPlays]

  def getPlays(userWallet: Wallet, filter: PlayFilter): IO[MarketPlayError, MarketPlays]

  def getPlays(userWallet: Wallet, filter: PlayFilter, contextId: ContextId): IO[MarketPlayError, MarketPlays]

  def getPosition(userId: UserId, playId: PlayId): IO[MarketPlayError, PositionDetails[Position]]

  def getNextPositions(playId: PlayId): IO[MarketPlayError, List[Position]]

  def getPreviousPositions(playId: PlayId): IO[MarketPlayError, List[Position]]

  def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit]

  def importPlays(userWallet: Wallet, startingFrom: Instant): IO[MarketPlayError, Unit]

  def extractTimeInterval(marketPlays: List[MarketPlay]): Option[TimeInterval] = {
    val timestamps = marketPlays.flatMap {
      case p: Position => p.entries.map(_.timestamp)
      case t: TopUp    => List(t.timestamp)
      case t: Withdraw => List(t.timestamp)
    }.sorted
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil          => None
    }
  }
}

object MarketPlayService {
  def getPlays(
    userWallet: Wallet
  )(filter: PlayFilter): ZIO[Has[MarketPlayService], MarketPlayError, MarketPlays] =
    ZIO.serviceWith[MarketPlayService](_.getPlays(userWallet, filter))
}

final case class LiveMarketPlayService(
  positionRepo: MarketPlayRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: BlockchainRepo,
  journalingRepo: JournalingRepo,
  currencyRepo: CurrencyRepo,
  playsCache: Cache[MarketPlay, MarketPlayError, MarketPlayData],
  logger: Logger[String]
) extends MarketPlayService {

  override def getPlays(userWallet: Wallet): IO[MarketPlayError, MarketPlays] =
    (for {
      marketPlays <- positionRepo.getPlays(userWallet.address).flatMap(enrichPlays)
    } yield MarketPlays(marketPlays)).orElseFail(MarketPlaysFetchError(s"Plays fetch error ${userWallet.address}"))

  override def getPlays(userWallet: Wallet, playFilter: PlayFilter): IO[MarketPlayError, MarketPlays] =
    (for {
      marketPlays    <- positionRepo.getPlays(userWallet.address).flatMap(enrichPlays)
      journalEntries <- journalingRepo.getEntries(userWallet.userId, marketPlays.map(_.id).values)
    } yield MarketPlays(withJournalEntries(marketPlays, journalEntries)))
      .orElseFail(MarketPlaysFetchError(s"Plays fetch error ${userWallet.address}"))

  //TODO I don't think I need to return Page from the repo, I could just return the plays.
  override def getPlays(
    userWallet: Wallet,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, MarketPlays] = ???
//    for {
//      page        <- positionRepo.getPlays(userWallet.address, filter, contextId)
//      quotes      <- getQuotesForMarketPlays(page.data)
//      enrichedPlays = marketPlays.map(play => addQuotes(play, quotes))
//      journalEntries <- journalingRepo.getEntries(
//                         userWallet.userId,
//                         marketPlays.map(_.id).values
//                       )
//    } yield MarketPlays(withJournalEntries(enrichedPlays, journalEntries).mostRecentFirst())

  private def withJournalEntries(plays: List[MarketPlay], entries: List[JournalEntry]): List[MarketPlay] = {
    val positionToEntryMap = entries.map(e => e.positionId.get -> e).toMap
    plays.map {
      case p: Position                  => p.copy(journal = p.id.flatMap(positionToEntryMap.get))
      case t @ (_: TopUp | _: Withdraw) => t
    }
  }

  override def getPosition(userId: UserId, positionId: PlayId): IO[MarketPlayError, PositionDetails[Position]] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    for {
      positionWithLinkIds <- positionRepo.getPosition(positionId)
      linkedPositions   <- positionRepo.getPositions(positionWithLinkIds.links.next ++ positionWithLinkIds.links.previous)
      (next, previous) = linkedPositions.partition(p => positionWithLinkIds.links.next.contains(p.id.get))
      nextEnriched     <- enrichPlays(next).map(_.asInstanceOf[List[Position]])
      previousEnriched <- enrichPlays(previous).map(_.asInstanceOf[List[Position]])
      position         <- enrichPlay(positionWithLinkIds.position).mapBoth(_.get, _.asInstanceOf[Position])
      journalEntry <- journalingRepo.getEntry(userId, position.id.get).map(Some(_)).catchSome {
                       case _: JournalNotFound => UIO.none
                     }
    } yield PositionDetails(position.copy(journal = journalEntry), PositionLinks(nextEnriched, previousEnriched))

  override def getNextPositions(playId: PlayId): IO[MarketPlayError, List[Position]] =
    for {
      _                         <- logger.info(s"Fetch next positions for position ${playId.value}")
      nextIds                   <- positionRepo.getNextPositionIds(playId)
      positions: List[Position] <- positionRepo.getPositions(nextIds).flatMap(enrichPlays).map(_.asInstanceOf[List[Position]])
    } yield positions

  override def getPreviousPositions(playId: PlayId): IO[MarketPlayError, List[Position]] =
    for {
      _          <- logger.info(s"Fetch previous positions for position ${playId.value}")
      prevIds    <- positionRepo.getPreviousPositionIds(playId)
      positions  <- positionRepo.getPositions(prevIds).flatMap(enrichPlays).map(_.asInstanceOf[List[Position]])
    } yield positions

  private def enrichPlays(plays: List[MarketPlay]): IO[MarketPlayError, List[MarketPlay]] =
    ZIO.collect(plays)(enrichPlay)

  private def enrichPlay(play: MarketPlay): IO[Option[MarketPlayError], MarketPlay] =
    playsCache
      .get(play)
      .flatMap(data =>
        play match {
          case p: Position if data.isInstanceOf[PositionData] =>
            UIO(p.copy(dataSource = Some(data.asInstanceOf[PositionData])))
          case t: TopUp if data.isInstanceOf[TopUpData] =>
            UIO(t.copy(topUpDataGenerator = Some(data.asInstanceOf[TopUpData])))
          case w: Withdraw if data.isInstanceOf[WithdrawData] =>
            UIO(w.copy(withdrawDataGenerator = Some(data.asInstanceOf[WithdrawData])))
          case _ =>
            ZIO.fail(Some(InvalidRepresentation(s"${play.getClass.getName} mismatch for ${data.getClass.getName}")))
        }
      )
      .orElseFail(Some(InvalidRepresentation("Unable to enrich play")))

  override def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit] =
    logger.info(s"Importing positions for ${userWallet.address}") *>
      importPlays(userWallet.address, blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPlays(userWallet: Wallet, startFrom: Instant): IO[MarketPlayError, Unit] =
    importPlays(userWallet.address, blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPlays(
    walletAddress: WalletAddress,
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: Wallet): IO[MarketPlayError, Unit] = {
    val noPlaysEffect = logger.info(s"No positions to import for ${userWallet.address}")

    @inline
    def handlePlayImport(plays: MarketPlays): IO[MarketPlayError, Unit] =
      for {
        //Get open positions that might become closed with the new data coming in
        openPositions <- positionRepo.getPositions(userWallet.address, Open).map(MarketPlays(_))
        merged        = openPositions.merge(plays)
        _ <- positionRepo
              .save(userWallet.address, merged.plays)
              .mapError(throwable => MarketPlayImportError(userWallet.address, throwable))
        _ <- logger.info(s"Data import complete for ${userWallet.address.value}")
      } yield ()

    for {
      _ <- logger.info(s"Importing data for ${userWallet.address.value}...")

      plays <- txStream.runCollect
                .mapBoth(
                  error => MarketPlayImportError(userWallet.address, new RuntimeException(error.message)),
                  chunks => findMarketPlays(walletAddress, chunks.toList)
                ) // TODO Try to optimize so as not to process the entire stream.

      _ <- if (plays.isEmpty) {
            noPlaysEffect
          } else {
            handlePlayImport(plays)
              .zipRight(
                currencyRepo
                  .upsert(plays.currencies)
                  .orElseFail(MarketPlayImportError(walletAddress, new RuntimeException("Currency upsert failed")))
              )
          }
    } yield ()
  }
}

object LiveMarketPlayService {
  val playData: MarketPlay => ZIO[Has[PriceQuoteRepo], MarketPlayError, MarketPlayData] = play => {
    ZIO.serviceWith[PriceQuoteRepo] { repo =>
      val (interval, currency) = play match {
        case p: Position => (p.timeInterval, p.currency)
        case t: TopUp    => (TimeInterval(t.timestamp, t.timestamp), Some(WBNB))
        case w: Withdraw => (TimeInterval(w.timestamp, w.timestamp), Some(WBNB))
      }

      (for {
        quotes <- currency.fold[IO[PriceQuoteError, PriceQuotes]](UIO(PriceQuotes.empty()))(c =>
                   repo.getQuotes(Set(c), interval).map(PriceQuotes(_))
                 )
        data = play match {
          case p: Position =>
            val pos = p.copy(dataSource = Some(PriceQuotePositionData(quotes)))
            PositionDataValues(
              pos.currency,
              pos.cost,
              pos.fees,
              pos.entryPrice,
              pos.exitPrice,
              pos.fiatSellValue,
              pos.balance(),
              pos.closedAt
            )
          case t: TopUp    => ???
          case w: Withdraw => ???
        }
      } yield data).orElseFail(MarketPlayNotFound(play.id.get))
    }
  }

  lazy val cacheLayer: ZLayer[Has[PriceQuoteRepo], Nothing, Has[Cache[MarketPlay, MarketPlayError, MarketPlayData]]] = Cache.make(1000, 1.day, lookup = Lookup(playData)).toLayer

  lazy val layer: URLayer[Has[MarketPlayRepo] with Has[PriceQuoteRepo] with Has[BlockchainRepo] with Has[
    JournalingRepo
  ] with Has[CurrencyRepo] with Has[Cache[MarketPlay, MarketPlayError, MarketPlayData]] with Logging, Has[
    MarketPlayService
  ]] =
    (LiveMarketPlayService(_, _, _, _, _, _, _)).toLayer
}
