package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.error._
import domain.blockchain.{ BlockchainRepo, Transaction }
import domain.model._
import domain.position.Position._
import domain.position.Positions.findMarketPlays
import domain.position.error._
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.PlayFilter

import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant

trait PositionService {
  def getPositions(userWallet: Wallet, filter: PlayFilter): IO[MarketPlayError, Positions]

  def getPositions(userWallet: Wallet, filter: PlayFilter, contextId: ContextId): IO[MarketPlayError, Positions]

  def getPosition(userId: UserId, positionId: PlayId): IO[MarketPlayError, Position]

  def importPositions(userWallet: Wallet): IO[MarketPlayError, Unit]

  def importPositions(userWallet: Wallet, startingFrom: Instant): IO[MarketPlayError, Unit]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp).sorted
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil          => None
    }
  }
}

object PositionService {
  def getPositions(
    userWallet: Wallet
  )(filter: PlayFilter): ZIO[Has[PositionService], MarketPlayError, Positions] =
    ZIO.serviceWith[PositionService](_.getPositions(userWallet, filter))
}

final case class LivePositionService(
                                      positionRepo: MarketPlayRepo,
                                      priceQuoteRepo: PriceQuoteRepo,
                                      blockchainRepo: BlockchainRepo,
                                      journalingRepo: JournalingRepo,
                                      logger: Logger[String]
) extends PositionService {
  override def getPositions(userWallet: Wallet, positionFilter: PlayFilter): IO[MarketPlayError, Positions] =
    for {
      positions <- positionRepo
                    .getPlays(userWallet.address, positionFilter)
                    .flatMap(enrichPositions)
                    .orElseFail(MarketPlaysFetchError(userWallet.address))
      journalEntries <- journalingRepo.getEntries(
                         userWallet.userId,
                         positions.map(_.id).collect { case Some(id) => id }
                       )
    } yield Positions(withJournalEntries(positions, journalEntries))

  override def getPositions(
                             userWallet: Wallet,
                             filter: PlayFilter,
                             contextId: ContextId
  ): IO[MarketPlayError, Positions] =
    for {
      positions <- positionRepo
                    .getPositions(userWallet.address, filter, contextId)
                    .flatMap(page => enrichPositions(page.data.items))
                    .orElseFail(MarketPlaysFetchError(userWallet.address))
      journalEntries <- journalingRepo.getEntries(
                         userWallet.userId,
                         positions.map(_.id).collect { case Some(id) => id }
                       )
    } yield Positions(withJournalEntries(positions, journalEntries))

  private def withJournalEntries(positions: List[Position], entries: List[JournalEntry]): List[Position] = {
    val positionToEntryMap = entries.map(e => e.positionId.get -> e).toMap
    positions.map(position => position.copy(journal = position.id.flatMap(positionToEntryMap.get)))
  }

  private def enrichPositions(positions: List[Position]): IO[MarketPlayError, List[Position]] = {
    val interval = extractTimeInterval(positions)
    if (positions.nonEmpty) {
      logger.info(s"Fetching quotes for [${interval.get.start} - ${interval.get.end}]") *>
      priceQuoteRepo
        .getQuotes(interval.get)
        .tap(quotes => logger.info(s"Found ${quotes.length} quotes"))
        .mapBoth(PriceQuotesError, PriceQuotes.apply)
        .map(priceQuotes =>
          positions.map(position => position.copy(priceQuotes = Some(priceQuotes.subset(position.timeInterval()))))
        )
    } else UIO(positions)
  }

  override def getPosition(userId: UserId, positionId: PlayId): IO[MarketPlayError, Position] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    positionRepo
      .getPlay(positionId)
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

  override def importPositions(userWallet: Wallet): IO[MarketPlayError, Unit] =
    logger.info(s"Importing positions for ${userWallet.address}") *>
      importPositions(blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPositions(userWallet: Wallet, startFrom: Instant): IO[MarketPlayError, Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPositions(
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: Wallet): IO[MarketPlayError, Unit] = {
    val noPositionsEffect = logger.info(s"No positions to import for ${userWallet.address}")

    @inline
    def handlePositionsImport(positions: List[Position]): IO[MarketPlayError, Unit] =
      for {
        //Get open positions that might become closed with the new data coming in
        openPositions <- positionRepo.getPlays(userWallet.address, Open).map(Positions(_))
        merged        = openPositions.merge(Positions(positions))
        _ <- positionRepo
              .save(userWallet.address, merged.items)
              .mapError(throwable => MarketPlayImportError(userWallet.address, throwable))
        _ <- logger.info(s"Data import complete for ${userWallet.address.value}")
      } yield ()

    for {
      _ <- logger.info(s"Importing data for ${userWallet.address.value}...")

      positions <- txStream.runCollect
                    .mapBoth(
                      error => MarketPlayImportError(userWallet.address, new RuntimeException(error.message)),
                      chunks => findMarketPlays(chunks.toList)
                    ) // TODO Try to optimize so as not to process the entire stream.

      _ <- if (positions.isEmpty) {
            noPositionsEffect
          } else {
            //TODO Pass in all positions, without the collect part.
            handlePositionsImport(positions.collect { case p: Position => p })
          }
    } yield ()
  }
}

object LivePositionService {
  lazy val layer: URLayer[Has[MarketPlayRepo] with Has[PriceQuoteRepo] with Has[BlockchainRepo] with Has[
    JournalingRepo
  ] with Logging, Has[
    PositionService
  ]] =
    (LivePositionService(_, _, _, _, _)).toLayer
}
