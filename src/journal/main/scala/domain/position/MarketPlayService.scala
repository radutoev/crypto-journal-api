package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.error._
import domain.blockchain.{ BlockchainRepo, Transaction }
import domain.model._
import domain.position.MarketPlays.findMarketPlays
import domain.position.error._
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import domain.wallet.Wallet
import util.MarketPlaysListOps
import vo.TimeInterval
import vo.filter.PlayFilter

import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant

trait MarketPlayService {
  def getPlays(userWallet: Wallet, filter: PlayFilter): IO[MarketPlayError, MarketPlays]

  def getPlays(userWallet: Wallet, filter: PlayFilter, contextId: ContextId): IO[MarketPlayError, MarketPlays]

  def getPosition(userId: UserId, playId: PlayId): IO[MarketPlayError, Position]

  def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit]

  def importPlays(userWallet: Wallet, startingFrom: Instant): IO[MarketPlayError, Unit]

  def extractTimeInterval(marketPlays: List[MarketPlay]): Option[TimeInterval] = {
    val timestamps = marketPlays.flatMap {
      case p: Position   => p.entries.map(_.timestamp)
      case t: TransferIn => List(t.timestamp)
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
  logger: Logger[String]
) extends MarketPlayService {
  override def getPlays(userWallet: Wallet, positionFilter: PlayFilter): IO[MarketPlayError, MarketPlays] =
    for {
      marketPlays <- positionRepo
                      .getPlays(userWallet.address, positionFilter)
                      .flatMap(enrichPlays)
                      .orElseFail(MarketPlaysFetchError(userWallet.address))
      journalEntries <- journalingRepo.getEntries(
                         userWallet.userId,
                         marketPlays.map(_.id).collect { case Some(id) => id }
                       )
    } yield MarketPlays(withJournalEntries(marketPlays, journalEntries).mostRecentFirst())

  override def getPlays(
    userWallet: Wallet,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, MarketPlays] =
    for {
      marketPlays <- positionRepo
                      .getPlays(userWallet.address, filter, contextId)
                      .flatMap(page => enrichPlays(page.data.plays))
                      .orElseFail(MarketPlaysFetchError(userWallet.address))
      journalEntries <- journalingRepo.getEntries(
                         userWallet.userId,
                         marketPlays.map(_.id).collect { case Some(id) => id }
                       )
    } yield MarketPlays(withJournalEntries(marketPlays, journalEntries).mostRecentFirst())

  private def withJournalEntries(plays: List[MarketPlay], entries: List[JournalEntry]): List[MarketPlay] = {
    val positionToEntryMap = entries.map(e => e.positionId.get -> e).toMap
    plays.map {
      case p: Position   => p.copy(journal = p.id.flatMap(positionToEntryMap.get))
      case t: TransferIn => t
    }
  }

  private def enrichPlays(marketPlays: List[MarketPlay]): IO[MarketPlayError, List[MarketPlay]] = {
    val interval = extractTimeInterval(marketPlays)
    if (marketPlays.nonEmpty) {
      logger.info(s"Fetching quotes for [${interval.get.start} - ${interval.get.end}]") *>
      priceQuoteRepo
        .getQuotes(interval.get)
        .tap(quotes => logger.info(s"Found ${quotes.length} quotes"))
        .mapBoth(PriceQuotesError, PriceQuotes.apply)
        .map(priceQuotes =>
          marketPlays.map {
            case p: Position => p.copy(priceQuotes = Some(priceQuotes.subset(p.timeInterval())))
            case t: TransferIn =>
              t.copy(priceQuotes =
                //TODO Extract TimeInterval generation
                Some(priceQuotes.subset(TimeInterval(t.timestamp.minusSeconds(3600), t.timestamp.plusSeconds(36000))))
              )
          }
        )
    } else UIO(marketPlays)
  }

  override def getPosition(userId: UserId, positionId: PlayId): IO[MarketPlayError, Position] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    positionRepo
      .getPosition(positionId)
      .flatMap(enrichPosition)
      .zipPar(journalingRepo.getEntry(userId, positionId).map(Some(_)).catchSome {
        case _: JournalNotFound => UIO.none
      })
      .mapBoth(_ => MarketPlayFetchError(positionId, new RuntimeException("Unable to enrich position")), {
        case (position, entry) => position.copy(journal = entry)
      })

  private def enrichPosition(position: Position): Task[Position] = {
    val interval = position.timeInterval()
    priceQuoteRepo
      .getQuotes(interval)
      .map(PriceQuotes.apply)
      .map(priceQuotes => position.copy(priceQuotes = Some(priceQuotes)))
  }

  override def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit] =
    logger.info(s"Importing positions for ${userWallet.address}") *>
      importPlays(blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPlays(userWallet: Wallet, startFrom: Instant): IO[MarketPlayError, Unit] =
    importPlays(blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPlays(
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: Wallet): IO[MarketPlayError, Unit] = {
    val noPlaysEffect = logger.info(s"No positions to import for ${userWallet.address}")

    @inline
    def handlePlayImport(plays: List[MarketPlay]): IO[MarketPlayError, Unit] =
      for {
        //Get open positions that might become closed with the new data coming in
        openPositions <- positionRepo.getPositions(userWallet.address, Open).map(MarketPlays(_))
        merged        = openPositions.merge(MarketPlays(plays))
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
                  chunks => findMarketPlays(chunks.toList)
                ) // TODO Try to optimize so as not to process the entire stream.

      _ <- if (plays.isEmpty) {
            noPlaysEffect
          } else {
            handlePlayImport(plays)
          }
    } yield ()
  }
}

object LiveMarketPlayService {
  lazy val layer: URLayer[Has[MarketPlayRepo] with Has[PriceQuoteRepo] with Has[BlockchainRepo] with Has[
    JournalingRepo
  ] with Logging, Has[
    MarketPlayService
  ]] =
    (LiveMarketPlayService(_, _, _, _, _)).toLayer
}
