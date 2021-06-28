package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{ EthBlockchainRepo, Transaction }
import domain.blockchain.error._
import domain.model._
import domain.position.error._
import domain.position.Position._
import domain.position.LivePositionService.findPositions
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import vo.TimeInterval

import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

trait PositionService {
  def getPositions(userWallet: UserWallet): IO[PositionError, Positions]

  def getPositions(userWallet: UserWallet, interval: TimeInterval): Task[List[Position]]

  def getPosition(positionId: PositionId): IO[PositionError, Position]

  def importPositions(userWallet: UserWallet): Task[Unit]

  def importPositions(userWallet: UserWallet, startingFrom: Instant): Task[Unit]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head, None))
      case head :: tail => Some(TimeInterval(head, Some(tail.last)))
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
  def getPositions(userWallet: UserWallet): ZIO[Has[PositionService], PositionError, Positions] =
    ZIO.serviceWith[PositionService](_.getPositions(userWallet))
}

final case class LivePositionService(
  positionRepo: PositionRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: EthBlockchainRepo,
  demoAccountConfig: DemoAccountConfig,
  logger: Logger[String]
) extends PositionService {
  override def getPositions(userWallet: UserWallet): IO[PositionError, Positions] =
    for {
      positions <- positionRepo
                    .getPositions(userWallet.address)(demoAccountConfig.maxPositions)
                    .flatMap(enrichPositions)
                    .orElseFail(PositionsFetchError(userWallet.address))
      checkpoint <- positionRepo.getCheckpoint(userWallet.address)
    } yield Positions(positions, checkpoint.latestTxTimestamp)

  override def getPositions(userWallet: UserWallet, interval: TimeInterval): Task[List[Position]] =
    positionRepo
      .getPositions(userWallet.address, interval)
      .flatMap(enrichPositions)

  override def getPosition(positionId: PositionId): IO[PositionError, Position] =
    positionRepo
      .getPosition(positionId)
      .flatMap(enrichPosition)
      .orElseFail(PositionFetchError(positionId, new RuntimeException("Unable to enrich position")))

  private def enrichPositions(positions: List[Position]): Task[List[Position]] = {
    val interval = extractTimeInterval(positions)
    if (positions.nonEmpty) {
      priceQuoteRepo
        .getQuotes(interval.get)
        .map(PriceQuotes.apply)
        .map(priceQuotes =>
          positions.map(position => position.copy(priceQuotes = Some(priceQuotes.subset(position.timeInterval()))))
        )
    } else UIO(positions)
  }

  private def enrichPosition(position: Position): Task[Position] = {
    val interval = position.timeInterval()
    priceQuoteRepo
      .getQuotes(interval)
      .map(PriceQuotes.apply)
      .map(priceQuotes => position.copy(priceQuotes = Some(priceQuotes)))
  }

  override def importPositions(userWallet: UserWallet): Task[Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPositions(userWallet: UserWallet, startFrom: Instant): Task[Unit] =
    importPositions(blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPositions(
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: UserWallet): Task[Unit] =
    for {
      _ <- logger.info(s"Importing data for ${userWallet.address.value}")
      positions <- txStream.runCollect
                    .orElseFail(new RuntimeException("Unable to fetch transactions")) //TODO Replace with domain error.
                    .map(chunks => findPositions(chunks.toList))                      // TODO Try to optimize so as not to process the entire stream.
      _ <- positionRepo.save(userWallet.address, positions).orElseFail(new RuntimeException("sss"))
      _ <- logger.info(s"Demo data import complete for ${userWallet.address.value}")
    } yield ()

  override def exists(address: WalletAddress): Task[Boolean] = positionRepo.exists(address)

  override def getCheckpoint(address: WalletAddress): IO[PositionError, Checkpoint] =
    positionRepo.getCheckpoint(address)
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo] with Has[EthBlockchainRepo] with Has[
    DemoAccountConfig
  ] with Logging, Has[
    PositionService
  ]] =
    (LivePositionService(_, _, _, _, _)).toLayer

  val TransactionTypes = Vector(Buy, Sell)

  def findPositions(transactions: List[Transaction]): List[Position] = {
    val transactionsByCoin = transactions
      .sortBy(_.instant)(Ordering[Instant])
      .filter(_.successful)
      .filter(_.hasTransactionEvents)
      .filter(tx => TransactionTypes.contains(tx.transactionType))
      .groupBy(_.coin.get)

    val positions = transactionsByCoin.flatMap {
      case (coin, txList) => {
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
            case Buy => Position(coin, Open, txList.head.instant, None, transactionsToPositionEntries(txList))
            case Sell =>
              Position(
                coin,
                Closed,
                txList.head.instant,
                Some(txList.last.instant),
                transactionsToPositionEntries(txList)
              )
          }
        }
      }
    }.toList

    positions.sortBy(_.openedAt)(Ordering[Instant].reverse)
  }

  val transactionsToPositionEntries: List[Transaction] => List[PositionEntry] =
    _.map(transactionToPositionEntry).collect {
      case Right(entry) => entry
    }

  val transactionToPositionEntry: Transaction => Either[String, PositionEntry] = tx => {
    for {
      value <- tx.value
      hash  <- TransactionHash(tx.hash)
    } yield PositionEntry(tx.transactionType, value, tx.fee, tx.instant, hash)
  }
}
