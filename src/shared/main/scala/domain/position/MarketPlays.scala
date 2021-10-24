package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model.{ Buy, Currency, FungibleData, Sell, TransactionHash, TransactionType, Unknown, WalletAddress }
import util.{ InstantOps, ListOps, MarketPlaysListOps }
import vo.TimeInterval

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

//most recent items first.
final case class MarketPlays(plays: List[MarketPlay]) {
  lazy val positions: List[Position]     = plays.positions
  lazy val transferIns: List[TransferIn] = plays.transferIns

  lazy val closedPositions: List[Position] = positions.filter(_.isClosed())
  lazy val openPositions: List[Position]   = positions.filter(_.isOpen())

  def merge(other: MarketPlays): MarketPlays = {
    var currencyPositionMap = Map.empty[Currency, Position]
    val otherPositions      = ArrayBuffer.empty[Position]

    other.positions.foreach { position =>
      if (currencyPositionMap.contains(position.currency)) {
        otherPositions.addOne(position)
      } else {
        currencyPositionMap += position.currency -> position
      }
    }

    //oldest first
    val merged = positions.reverse.map { position =>
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

    MarketPlays(
      (otherPositions.toList ::: notCorrelated ::: merged ::: transferIns ::: other.transferIns)
        .sortBy(_.openedAt)(Ordering[Instant])
    )
  }

  def filter(interval: TimeInterval): MarketPlays =
    MarketPlays(positions.filter(_.inInterval(interval)))

  def trend(of: Position => Option[FungibleData]): List[FungibleData] =
    positions.headOption.map(_.openedAt).fold[List[FungibleData]](List.empty) { openedAt =>
      val interval = TimeInterval(openedAt.atBeginningOfDay(), Instant.now()) //should be an implicit
      interval.days().map(day => filter(TimeInterval(interval.start, day)).positions.map(of).sumFungibleData())
    }
}

object MarketPlays {
  def apply(items: List[MarketPlay]): MarketPlays = new MarketPlays(items)

  def empty(): MarketPlays = MarketPlays(List.empty)

  val TransactionTypes = Vector(Buy, Sell)

  //TODO I should return MarketPlays here.
  def findMarketPlays(wallet: WalletAddress, transactions: List[Transaction]): List[MarketPlay] = {
    val successes = transactions
      .sortBy(_.instant)(Ordering[Instant])
      .filter(_.successful)

    val (txWithEvents, txWithoutEvents) = successes.partition(_.hasTransactionEvents)

    val transactionsByCoin = txWithEvents
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

    val (incoming, outgoing) = txWithoutEvents.partition(_.toAddress == wallet.value)

    val transferIns = incoming.map(transactionToTransferIn).rights

    val transferOuts = outgoing.map(transactionToTransferOut).rights

    (positions ++ transferIns ++ transferOuts).sortBy(_.openedAt)(Ordering[Instant].reverse)
  }

  val transactionToPositionEntry: Transaction => Either[String, PositionEntry] = tx => {
    for {
      value <- tx.value
      hash  <- TransactionHash(tx.hash)
    } yield PositionEntry(tx.transactionType, value, tx.fee, tx.instant, hash)
  }

  val transactionsToPositionEntries: List[Transaction] => List[PositionEntry] =
    _.map(transactionToPositionEntry).rights

  def transactionToTransferIn(tx: Transaction): Either[String, TransferIn] =
    for {
      value <- tx.value
      hash  <- TransactionHash(tx.hash)
    } yield TransferIn(hash, value, tx.fee, tx.instant)

  def transactionToTransferOut(tx: Transaction): Either[String, TransferOut] =
    for {
      value <- tx.value
      hash  <- TransactionHash(tx.hash)
    } yield TransferOut(hash, value, tx.fee, tx.instant)
}
