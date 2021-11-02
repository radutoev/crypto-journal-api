package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model.{ Currency, FungibleData, TransactionHash, TransactionType, Unknown, WBNB, WalletAddress }
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
  lazy val transferIns: List[TopUp]            = plays.transferIns
  lazy val transferOuts: List[TransferOutPlay] = plays.transferOuts

  lazy val closedPositions: List[Position] = positions.filter(_.isClosed())
  lazy val openPositions: List[Position]   = positions.filter(_.isOpen())

  def merge(other: MarketPlays): MarketPlays =
    ???
//    var currencyPositionMap = Map.empty[Currency, Position]
//    val otherPositions      = ArrayBuffer.empty[Position]
//
//    other.positions.foreach { position =>
//      if (currencyPositionMap.contains(position.currency)) {
//        otherPositions.addOne(position)
//      } else {
//        currencyPositionMap += position.currency -> position
//      }
//    }
//
//    //oldest first
//    val merged = positions.reverse.map { position =>
//      if (currencyPositionMap.contains(position.currency)) {
//        val oldPosition = currencyPositionMap(position.currency)
//        oldPosition.copy(
//          entries = oldPosition.entries ::: position.entries
//        )
//        currencyPositionMap -= position.currency
//        oldPosition
//      } else {
//        position
//      }
//    }
//
//    val notCorrelated = currencyPositionMap.values.toList
//
//    MarketPlays(
//      (otherPositions.toList :::
//        notCorrelated :::
//        merged :::
//        transferIns :::
//        other.transferIns :::
//        transferOuts :::
//        other.transferOuts).sortBy(_.openedAt)(Ordering[Instant])
//    )

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

  //TODO I should return MarketPlays here.
  def findMarketPlays(wallet: WalletAddress, transactions: List[Transaction]): List[MarketPlay] = {
    val incoming: mutable.Map[Currency, ListBuffer[PositionEntry]]                = mutable.Map.empty
    val incomingByContract: mutable.Map[WalletAddress, ListBuffer[PositionEntry]] = mutable.Map.empty
    val playsBuffer: ListBuffer[MarketPlay]                                       = ListBuffer.empty
    val topUpsBuffer: ListBuffer[TopUp]                                           = ListBuffer.empty

    @inline
    def addToIncoming(currency: Currency, entry: PositionEntry): Unit =
      if (incoming.contains(currency)) {
        incoming(currency).addOne(entry)
      } else {
        incoming.put(currency, ListBuffer(entry))
      }

    @inline
    def addAllToIncoming(currency: Currency, entries: ListBuffer[PositionEntry]): Unit =
      if (incoming.contains(currency)) {
        incoming(currency).addAll(ListBuffer.from(entries.toList))
      } else {
        incoming.put(currency, ListBuffer.from(entries))
      }

    @inline
    def addToContractIncoming(walletAddress: WalletAddress, entry: PositionEntry): Unit =
      if (incomingByContract.contains(walletAddress)) {
        incomingByContract(walletAddress).addOne(entry)
      } else {
        incomingByContract.put(walletAddress, ListBuffer(entry))
      }

    val entries = transactions
      .filter(_.successful)
      .flatMap(tx =>
        PositionEntry.fromTransaction(tx, wallet) match {
          case Right(list) => list
        }
      )
      .sortBy(_.timestamp)(Ordering[Instant])

    entries.foreach {
      case airDrop: AirDrop =>
        addToIncoming(airDrop.received.currency, airDrop)

      case approval: Approval =>
        addToContractIncoming(approval.forContract, approval)

      case buy: Buy =>
        addToIncoming(buy.received.currency, buy)

      case claim: Claim =>
        val itemsToAdd = incomingByContract.getOrElse(claim.receivedFrom, ListBuffer.empty)
        itemsToAdd.addOne(claim)
        addAllToIncoming(claim.received.currency, itemsToAdd)
        itemsToAdd.clear()

      case contrib: Contribute =>
        addToContractIncoming(contrib.to, contrib)

      case sell: Sell =>
        val entries = incoming.getOrElse(sell.sold.currency, ListBuffer.empty)
        entries.addOne(sell)
        val position = Position(entries = entries.toList)
        playsBuffer.addOne(position)
        entries.clear()

      case transferIn: TransferIn =>
        addToIncoming(transferIn.value.currency, transferIn)

      case transferOut: TransferOut => ???
    }

    //TODO Handle the rest of the items that didn't have a closing Sell.
    incoming.foreach {
      case (currency, transferIns) if currency == WBNB && transferIns.nonEmpty =>
        topUpsBuffer.addAll(transferIns.asInstanceOf[ListBuffer[TransferIn]].map(t => TopUp(t.hash, t.value, t.fee, t.timestamp)))
      case _ =>
    }

    (playsBuffer.toList ::: topUpsBuffer.toList)
      .sortBy(_.openedAt)(Ordering[Instant])
  }
}
