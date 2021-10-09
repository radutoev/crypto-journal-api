package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.error._
import domain.blockchain.{BlockchainRepo, Transaction}
import domain.model._
import domain.position.Position._
import domain.position.Positions.findPositions
import domain.position.error._
import domain.pricequote.{PriceQuoteRepo, PriceQuotes}
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.PositionFilter

import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Has, IO, Task, UIO, URLayer, ZIO}

import java.time.Instant

trait PositionService {
  def getPositions(userWallet: Wallet)(filter: PositionFilter): IO[PositionError, Positions]

  def getPosition(userId: UserId, positionId: PositionId): IO[PositionError, Position]

  def importPositions(userWallet: Wallet): IO[PositionError, Unit]

  def importPositions(userWallet: Wallet, startingFrom: Instant): IO[PositionError, Unit]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head))
      case head :: tail => Some(TimeInterval(tail.last, head))
      case Nil          => None
    }
  }
}

object PositionService {
  def getPositions(
    userWallet: Wallet
  )(filter: PositionFilter): ZIO[Has[PositionService], PositionError, Positions] =
    ZIO.serviceWith[PositionService](_.getPositions(userWallet)(filter))
}

final case class LivePositionService(
  positionRepo: PositionRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: BlockchainRepo,
  journalingRepo: JournalingRepo,
  logger: Logger[String]
) extends PositionService {
  override def getPositions(userWallet: Wallet)(positionFilter: PositionFilter): IO[PositionError, Positions] = {
    def withJournalEntries(positions: List[Position], entries: List[JournalEntry]): List[Position] = {
      val positionToEntryMap = entries.map(e => e.positionId.get -> e).toMap
      positions.map(position => position.copy(journal = position.id.flatMap(positionToEntryMap.get)))
    }

    for {
      positions <- positionRepo
                    .getPositions(userWallet.address)(positionFilter)
                    .flatMap(enrichPositions)
                    .orElseFail(PositionsFetchError(userWallet.address))
      journalEntries <- journalingRepo.getEntries(
                         userWallet.userId,
                         positions.map(_.id).collect { case Some(id) => id }
                       )
    } yield Positions(withJournalEntries(positions, journalEntries))
  }

  private def enrichPositions(positions: List[Position]): IO[PositionError, List[Position]] = {
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

  override def getPosition(userId: UserId, positionId: PositionId): IO[PositionError, Position] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    positionRepo
      .getPosition(positionId)
      .flatMap(enrichPosition)
      .zipPar(journalingRepo.getEntry(userId, positionId).map(Some(_)).catchSome {
        case _: JournalNotFound => UIO.none
      })
      .mapBoth(_ => PositionFetchError(positionId, new RuntimeException("Unable to enrich position")), {
        case (position, entry) => position.copy(journal = entry)
      })

  private def enrichPosition(position: Position): Task[Position] = {
    val interval = position.timeInterval()
    priceQuoteRepo
      .getQuotes(interval)
      .map(PriceQuotes.apply)
      .map(priceQuotes => position.copy(priceQuotes = Some(priceQuotes)))
  }

  override def importPositions(userWallet: Wallet): IO[PositionError, Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPositions(userWallet: Wallet, startFrom: Instant): IO[PositionError, Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPositions(
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: Wallet): IO[PositionError, Unit] = {
    val noPositionsEffect = logger.info(s"No positions to import for ${userWallet.address}")

    @inline
    def handlePositionsImport(positions: List[Position]): IO[PositionError, Unit] =
      for {
        //Get open positions that might become closed with the new data coming in
        openPositions <- positionRepo.getPositions(userWallet.address, Open).map(Positions(_))
        merged        = openPositions.merge(Positions(positions))
        _ <- positionRepo
              .save(userWallet.address, merged.items)
              .mapError(throwable => PositionImportError(userWallet.address, throwable))
        _ <- logger.info(s"Data import complete for ${userWallet.address.value}")
      } yield ()

    for {
      _ <- logger.info(s"Importing data for ${userWallet.address.value}...")

      positions <- txStream.runCollect
                    .mapBoth(
                      error => PositionImportError(userWallet.address, new RuntimeException(error.message)),
                      chunks => findPositions(chunks.toList)
                    ) // TODO Try to optimize so as not to process the entire stream.

      _ <- if (positions.isEmpty) {
            noPositionsEffect
          } else {
            handlePositionsImport(positions)
          }
    } yield ()
  }
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo] with Has[BlockchainRepo] with Has[
    JournalingRepo
  ] with Logging, Has[
    PositionService
  ]] =
    (LivePositionService(_, _, _, _, _)).toLayer
}
