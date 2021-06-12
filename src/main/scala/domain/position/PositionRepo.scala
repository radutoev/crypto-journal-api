package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain._
import domain.model.{Closed, Open, TransactionTypes}
import domain.position.LivePositionRepo.findPositions

import zio.{Function1ToLayerSyntax, Has, Task, URLayer, ZIO}

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

trait PositionRepo {
  def getPositions(wallet: String): Task[List[Position]]
}

object PositionRepo {
  def getPositions(wallet: String): ZIO[Has[PositionRepo], Throwable, List[Position]] =
    ZIO.serviceWith[PositionRepo](_.getPositions(wallet))
}

final case class LivePositionRepo(ethBlockchainRepo: EthBlockchainRepo) extends PositionRepo {
  override def getPositions(wallet: String): Task[List[Position]] = {
    ethBlockchainRepo.fetchTransactions(wallet).map(findPositions)
  }
}

object LivePositionRepo {
  val layer: URLayer[Has[EthBlockchainRepo], Has[PositionRepo]] = (LivePositionRepo(_)).toLayer

  //TODO Move this to domain after defining Erc-20 or ETH model that I can maybe instantiate from covalent facade.
  def findPositions(transactions: List[Transaction]): List[Position] = {
    val transactionsByCoin = transactions.sortBy(_.instant)(Ordering[Instant])
      .filter(_.successful)
      .filter(_.hasTransactionEvents)
      .filter(tx => TransactionTypes.contains(tx.transactionType))
      .groupBy(_.coin.get)

    val positions = transactionsByCoin.flatMap {
      case (coin, txList) => {
        val grouped: ArrayBuffer[List[Transaction]] = ArrayBuffer.empty

        val acc: ArrayBuffer[Transaction] = ArrayBuffer.empty
        var lastTxType: TransactionType = Unknown //just a marker to get things going.

        for(tx <- txList) {
          tx.transactionType match {
            case Buy =>
              if(lastTxType == Buy) {
                acc.addOne(tx)
              } else if(lastTxType == Sell) {
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

        if(acc.nonEmpty) {
          grouped.addOne(acc.toList)
        }

        grouped.toList.map { txList =>
          txList.last.transactionType match {
            case Buy => Position(coin, Open, txList.head.instant, None, txList.map(_.hash))
            case Sell => Position(coin, Closed, txList.head.instant, Some(txList.last.instant), txList.map(_.hash))
          }
        }
      }
    }.toList

    positions.sortBy(_.openedAt)(Ordering[Instant].reverse)
  }
}
