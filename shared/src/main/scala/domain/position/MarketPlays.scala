package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model.{ Currency, FungibleData, TransactionHash, TransactionType, Unknown, WalletAddress }
import util.{ InstantOps, ListEitherOps, MarketPlaysListOps }
import vo.TimeInterval

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty

import java.time.Instant
import scala.collection.mutable
import scala.collection.mutable.{ ArrayBuffer, ListBuffer }

//most recent items first.
final case class MarketPlays(plays: List[MarketPlay]) {
  lazy val positions: List[Position]           = plays.positions
  lazy val transferIns: List[TransferInPlay]   = plays.transferIns
  lazy val transferOuts: List[TransferOutPlay] = plays.transferOuts

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
      (otherPositions.toList :::
        notCorrelated :::
        merged :::
        transferIns :::
        other.transferIns :::
        transferOuts :::
        other.transferOuts).sortBy(_.openedAt)(Ordering[Instant])
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

//  val TransactionTypes = Vector(Buy, Contribute, Claim, Sell)

  //TODO I should return MarketPlays here.
  //TODO Refactor and test this.
  def findMarketPlays(wallet: WalletAddress, transactions: List[Transaction]): List[MarketPlay] =
    ???
//    val successes = transactions
//      .sortBy(_.instant)(Ordering[Instant])
//      .filter(_.successful)
//
//    val (txWithEvents, txWithoutEvents) = successes.partition(_.hasTransactionEvents)
//
//    val (txToProcess, contributions) = txWithEvents.partition(_.transactionType != Contribute)
//
//    //Build possible positions by looking for contributions.
//    //I am grouping them by address in chronological order.
//    val contributionsByAddress = mutable.Map.from(
//      contributions
//        .groupBy(_.toAddress)
//        .view
//        .mapValues(ListBuffer.from(_))
//    )
//
//    //Group all transactions by coin symbol.
//    val transactionsByCoin = txToProcess.groupBy(_.coin)
//
//    //Generate positions from transactions that have coin information.
//    //This takes a chronological view of the buy/sell transactions and attempts to build either open or closed Positions.
//    val positions = transactionsByCoin.flatMap {
//      case (Some(rawCurrency), txList) =>
//        val currency                                = refined.refineV[NonEmpty].unsafeFrom(rawCurrency)
//        val grouped: ArrayBuffer[List[Transaction]] = ArrayBuffer.empty
//
//        val acc: ArrayBuffer[Transaction] = ArrayBuffer.empty
//        var lastTxType: TransactionType   = Unknown //just a marker to get things going.
//
//        for (tx <- txList) {
//          tx.transactionType match {
//            case Approval =>
//              if(lastTxType == Buy || lastTxType == Claim) {
//                acc.addOne(tx)
//              } else {
//                println(s"Approval before Buy on transaction ${tx.hash}")
//              }
//            case Buy =>
//              if (lastTxType == Buy) {
//                acc.addOne(tx)
//              } else if(lastTxType == Approval) {
//                println(s"Buy after Approval on transaction ${tx.hash}")
//              } else if (lastTxType == Sell) {
//                grouped.addOne(acc.toList)
//                acc.clear()
//                acc.addOne(tx)
//              } else {
//                acc.addOne(tx)
//              }
//            case Sell =>
//              if(lastTxType == Approval) {
//                acc.addOne(tx)
//              } else {
//                println(s"Sell without prior Approval detected for transaction ${tx.hash}")
//              }
//            case Claim =>
//              //this can be preceded by an optional set of contributions.
//              if (lastTxType == Claim) {
//                acc.addOne(tx)
//              } else {
//                val toAddress = tx.toAddress
//                val contributions =
//                  contributionsByAddress.getOrElse(toAddress, List.empty).filter(_.instant.isBefore(tx.instant))
//                acc.addAll(contributions :+ tx)
//                //remove contributions made before this claim.
//                if (contributions.nonEmpty) {
//                  contributionsByAddress(toAddress) --= contributions
//                  if (contributionsByAddress(toAddress) == Nil) {
//                    contributionsByAddress -= toAddress
//                  }
//                }
//              }
//            case Unknown => //do nothing
//              println(
//                tx.hash + ";" + tx.transactionType + ";" + tx.coin
//                  .getOrElse("") + ";" + tx.value.map(_.amount.toString()).getOrElse("")
//              )
//          }
//          lastTxType = tx.transactionType
//        }
//
//        if (acc.nonEmpty) {
//          grouped.addOne(acc.toList)
//        }
//
//        //List of transactions is made of Sells, Buys, Claims or Contributions.
//        grouped.toList.map(txList => Position(currency, txList.head.instant, transactionsToPositionEntries(txList)))
//
//      case (None, txList) =>
//        println("No coin")
//        txList.foreach(tx =>
//          println(
//            tx.hash + ";" + tx.transactionType + ";" + tx.coin
//              .getOrElse("") + ";" + tx.value.map(_.amount.toString()).getOrElse("")
//          )
//        )
//        List.empty
//    }.toList
//
//    val transferIns = txWithoutEvents.filter(_.toAddress == wallet.value).map(transactionToTransferIn).rights
//
//    val transferOuts = txWithoutEvents.filter(_.fromAddress == wallet.value).map(transactionToTransferOut).rights
//
//    (positions ++ transferIns ++ transferOuts).sortBy(_.openedAt)(Ordering[Instant].reverse)

//  val transactionToPositionEntry: Transaction => Either[String, PositionEntry] = tx => {
//    for {
//      value <- tx.value
//      hash  <- TransactionHash(tx.hash)
//    } yield PositionEntry(tx.transactionType, value, tx.fee, tx.instant, hash)
//  }
//
//  val transactionsToPositionEntries: List[Transaction] => List[PositionEntry] =
//    _.map(transactionToPositionEntry).rights
//
//  def transactionToTransferIn(tx: Transaction): Either[String, TransferInPlay] =
//    for {
//      value <- tx.value
//      hash  <- TransactionHash(tx.hash)
//    } yield TransferIn(hash, value, tx.fee, tx.instant)
//
//  def transactionToTransferOut(tx: Transaction): Either[String, TransferOutPlay] =
//    for {
//      value <- tx.value
//      hash  <- TransactionHash(tx.hash)
//    } yield TransferOut(hash, value, tx.fee, tx.instant)
}
