package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{ EthBlockchainRepo, Transaction }
import domain.blockchain.error._
import domain.model._
import domain.position.error._
import domain.position.Position._
import domain.position.LivePositionService.findPositions
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import vo.{ JournalPosition, TimeInterval }
import vo.filter.PositionFilter

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty
import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

trait PositionService {
  def getPositions(userWallet: UserWallet)(filter: PositionFilter): IO[PositionError, Positions]

  def getPosition(userId: UserId, positionId: PositionId): IO[PositionError, JournalPosition]

  def diff(userWallet: UserWallet): IO[PositionError, Positions]

  def importPositions(userWallet: UserWallet): IO[PositionError, Unit]

  def importPositions(userWallet: UserWallet, startingFrom: Instant): IO[PositionError, Unit]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil          => None
    }
  }

  /**
   * Checks if the system is aware of the given address.
   *
   * @param address to lookup
   * @return true if system is aware of the wallet address, false otherwise.
   */
  def exists(address: WalletAddress): Task[Boolean]

  def getCheckpoint(address: WalletAddress): IO[PositionError, Checkpoint]
}

object PositionService {
  def getPositions(
    userWallet: UserWallet
  )(filter: PositionFilter): ZIO[Has[PositionService], PositionError, Positions] =
    ZIO.serviceWith[PositionService](_.getPositions(userWallet)(filter))
}

final case class LivePositionService(
  positionRepo: PositionRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: EthBlockchainRepo,
  journalingRepo: JournalingRepo,
  demoAccountConfig: DemoAccountConfig,
  logger: Logger[String]
) extends PositionService {
  override def getPositions(userWallet: UserWallet)(positionFilter: PositionFilter): IO[PositionError, Positions] =
    for {
      positions <- positionRepo
                    .getPositions(userWallet.address)(positionFilter)
                    .flatMap(enrichPositions)
                    .orElseFail(PositionsFetchError(userWallet.address))
      result <- positionRepo
                 .getCheckpoint(userWallet.address)
                 .map(checkpoint => Positions(positions, checkpoint.latestTxTimestamp))
                 .catchSome {
                   case CheckpointNotFound(_) => UIO(Positions.empty())
                 }
    } yield result

  private def getPositions(userWallet: UserWallet, startFrom: Instant): IO[PositionError, Positions] =
    positionRepo
      .getPositions(userWallet.address, startFrom)
      .flatMap(enrichPositions)
      .map(positions => Positions(positions, Some(startFrom)))

  private def enrichPositions(positions: List[Position]): IO[PositionError, List[Position]] = {
    val interval = extractTimeInterval(positions)
    if (positions.nonEmpty) {
      priceQuoteRepo
        .getQuotes(interval.get)
        .bimap(PriceQuotesError, PriceQuotes.apply)
        .map(priceQuotes =>
          positions.map(position => position.copy(priceQuotes = Some(priceQuotes.subset(position.timeInterval()))))
        )
    } else UIO(positions)
  }

  override def getPosition(userId: UserId, positionId: PositionId): IO[PositionError, JournalPosition] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    positionRepo
      .getPosition(positionId)
      .flatMap(enrichPosition)
      .zipPar(journalingRepo.getEntry(userId, positionId).map(Some(_)).catchSome {
        case _: JournalNotFound => UIO.none
      })
      .bimap(_ => PositionFetchError(positionId, new RuntimeException("Unable to enrich position")), {
        case (position, entry) => JournalPosition(position, entry)
      })

  private def enrichPosition(position: Position): Task[Position] = {
    val interval = position.timeInterval()
    priceQuoteRepo
      .getQuotes(interval)
      .map(PriceQuotes.apply)
      .map(priceQuotes => position.copy(priceQuotes = Some(priceQuotes)))
  }

  override def diff(userWallet: UserWallet): IO[PositionError, Positions] = {
    @inline
    def diffPositions(timestamp: Instant): IO[PositionError, Positions] =
      importPositions(userWallet, timestamp) *> getPositions(userWallet, timestamp)

    def noNewPositions(): UIO[Positions] =
      logger.debug(s"No new positions found for ${userWallet.address}") *> UIO(Positions.empty())

    for {
      checkpoint <- getCheckpoint(userWallet.address)
      positions  <- checkpoint.latestTxTimestamp.fold[IO[PositionError, Positions]](noNewPositions())(diffPositions)
    } yield positions
  }

  override def importPositions(userWallet: UserWallet): IO[PositionError, Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPositions(userWallet: UserWallet, startFrom: Instant): IO[PositionError, Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPositions(
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: UserWallet): IO[PositionError, Unit] = {
    val noPositionsEffect = logger.debug(s"No positions to import for ${userWallet.address}")

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
                    .bimap(
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

  override def exists(address: WalletAddress): Task[Boolean] = positionRepo.exists(address)

  override def getCheckpoint(address: WalletAddress): IO[PositionError, Checkpoint] =
    positionRepo.getCheckpoint(address)
}

object LivePositionService {
  lazy val layer
    : URLayer[Has[PositionRepo] with Has[PriceQuoteRepo] with Has[EthBlockchainRepo] with Has[JournalingRepo] with Has[
      DemoAccountConfig
    ] with Logging, Has[
      PositionService
    ]] =
    (LivePositionService(_, _, _, _, _, _)).toLayer

  val TransactionTypes = Vector(Buy, Sell)

  def findPositions(transactions: List[Transaction]): List[Position] = {
    val transactionsByCoin = transactions
      .sortBy(_.instant)(Ordering[Instant])
      .filter(_.successful)
      .filter(_.hasTransactionEvents)
      .filter(tx => TransactionTypes.contains(tx.transactionType))
      .groupBy(_.coin.get)

    val positions = transactionsByCoin.flatMap {
      case (rawCurrency, txList) =>
        val currency                                = refined.refineV[NonEmpty].unsafeFrom(rawCurrency)
        val grouped: ArrayBuffer[List[Transaction]] = ArrayBuffer.empty

        val acc: ArrayBuffer[Transaction] = ArrayBuffer.empty
        var lastTxType: TransactionType   = Unknown //just a marker to get things going.

        for (tx <- txList) {
          tx.transactionType match {
            case Buy =>
              if (lastTxType == Buy) {
                acc.addOne(tx)
              } else if (lastTxType == Sell) {
                grouped.addOne(acc.toList)
                acc.clear()
                acc.addOne(tx)
              } else {
                acc.addOne(tx)
              }
            case Sell =>
              acc.addOne(tx)
            case Unknown => //do nothing
          }
          lastTxType = tx.transactionType
        }

        if (acc.nonEmpty) {
          grouped.addOne(acc.toList)
        }

        grouped.toList.map { txList =>
          txList.last.transactionType match {
            case Buy => Position(currency, txList.head.instant, transactionsToPositionEntries(txList))
            case Sell =>
              Position(
                currency,
                txList.head.instant,
                transactionsToPositionEntries(txList)
              )
          }
        }
    }.toList

    positions.sortBy(_.openedAt)(Ordering[Instant].reverse)
  }

  val transactionToPositionEntry: Transaction => Either[String, PositionEntry] = tx => {
    for {
      value <- tx.value
      hash  <- TransactionHash(tx.hash)
    } yield PositionEntry(tx.transactionType, value, tx.fee, tx.instant, hash)
  }

  val transactionsToPositionEntries: List[Transaction] => List[PositionEntry] =
    _.map(transactionToPositionEntry).collect {
      case Right(entry) => entry
    }
}
