package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model.{ Buy, Currency, Sell, TransactionHash, TransactionType, Unknown }
import vo.TimeInterval

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

//most recent items first.
final case class Positions(items: List[Position]) {
  lazy val closedPositions: List[Position] = items.filter(_.isClosed())
  lazy val openPositions: List[Position]   = items.filter(_.isOpen())

  def merge(other: Positions): Positions = {
    var currencyPositionMap = Map.empty[Currency, Position]
    val otherItems          = ArrayBuffer.empty[Position]

    other.items.foreach { position =>
      if (currencyPositionMap.contains(position.currency)) {
        otherItems.addOne(position)
      } else {
        currencyPositionMap += position.currency -> position
      }
    }

    //oldest first
    val merged = items.reverse.map { position =>
      if (currencyPositionMap.contains(position.currency)) {
        val oldPosition = currencyPositionMap(position.currency)
        oldPosition.copy(
          entries = oldPosition.entries ::: position.entries
        )
        currencyPositionMap -= position.currency
        oldPosition
      } else {
        position
      }
    }

    val notCorrelated = currencyPositionMap.values.toList

    Positions((otherItems.toList ::: notCorrelated ::: merged).sortBy(_.openedAt)(Ordering[Instant]))
  }

  def filter(interval: TimeInterval): Positions =
    Positions(items.filter(_.inInterval(interval)))
}

object Positions {
  def apply(items: List[Position]): Positions = new Positions(items)

  def empty(): Positions = Positions(List.empty)

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
