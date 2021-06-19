package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{ EthBlockchainRepo, Transaction }
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import domain.model._
import vo.TimeInterval

import zio.logging.{ Logger, Logging }
import zio.{ Function2ToLayerSyntax, Has, Task, URLayer, ZIO }

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

trait PositionService {
  def getPositions(walletAddress: String): Task[List[Position]]

  def importPositions(userId: UserId, address: WalletAddress): Task[Unit]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head, None))
      case head :: tail => Some(TimeInterval(head, Some(tail.last)))
      case Nil          => None
    }
  }
}

object PositionService {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[Position]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}

final case class LivePositionService(
  positionRepo: PositionRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: EthBlockchainRepo,
  logger: Logger[String]
) extends PositionService {
//  override def getPositions(walletAddress: String): Task[List[Position]] =
//    for {
//      positions   <- positionRepo.getPositions(walletAddress)
//      interval    = extractTimeInterval(positions)
//      priceQuotes <- priceQuoteRepo.getQuotes(interval.get).map(PriceQuotes.apply)
//      enrichedPositions = positions.map(position =>
//        position.copy(priceQuotes = Some(priceQuotes.subset(position.timeInterval())))
//      )
//    } yield enrichedPositions

  override def getPositions(walletAddress: String): Task[List[Position]] = ???

  //demo for now
  override def importPositions(userId: UserId, address: WalletAddress): Task[Unit] =
    for {
      _         <- logger.info(s"Importing demo data for ${address.value}")
      positions <- blockchainRepo.fetchTransactions(address).map(findPositions)
      _         <- positionRepo.save(userId, address, positions)
      _         <- logger.info(s"Demo data import complete for ${address.value}")
    } yield ()

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

  val transactionToPositionEntry: Transaction => Either[String, PositionEntry] = tx =>
    tx.value.map(value => PositionEntry(tx.transactionType, value, tx.fee, tx.instant))
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo] with Has[EthBlockchainRepo] with Logging, Has[
    PositionService
  ]] =
    (LivePositionService(_, _, _, _)).toLayer
}
