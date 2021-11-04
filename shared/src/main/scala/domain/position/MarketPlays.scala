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

  lazy val closedPositions: List[Position] = positions.filter(_.isClosed)
  lazy val openPositions: List[Position]   = positions.filter(_.isOpen)

  //TODO implement merge.
  def merge(other: MarketPlays): MarketPlays = other
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

  def isEmpty: Boolean = plays.isEmpty

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

  def findMarketPlays(wallet: WalletAddress, transactions: List[Transaction]): MarketPlays = {
    val currencyBuffer: mutable.Map[Currency, ListBuffer[PositionEntry]]          = mutable.Map.empty
    val incomingByContract: mutable.Map[WalletAddress, ListBuffer[PositionEntry]] = mutable.Map.empty
    val playsBuffer: ListBuffer[MarketPlay]                                       = ListBuffer.empty
    val topUpsBuffer: ListBuffer[TopUp]                                           = ListBuffer.empty

    @inline
    def addToCurrencyBuffer(currency: Currency, entry: PositionEntry): Unit =
      if (currencyBuffer.contains(currency)) {
        currencyBuffer(currency).addOne(entry)
      } else {
        currencyBuffer.put(currency, ListBuffer(entry))
      }

    @inline
    def addAllToCurrencyBuffer(currency: Currency, entries: ListBuffer[PositionEntry]): Unit =
      if (currencyBuffer.contains(currency)) {
        currencyBuffer(currency).addAll(ListBuffer.from(entries.toList))
      } else {
        currencyBuffer.put(currency, ListBuffer.from(entries))
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
          case Left(reason) =>
            println(reason) //TODO Maybe have an accumulator of errors to log by the invoker of the method.
            List.empty
          case Right(list) => list
        }
      )
      .sortBy(_.timestamp)(Ordering[Instant])

    entries.foreach {
      case airDrop: AirDrop =>
        addToCurrencyBuffer(airDrop.received.currency, airDrop)

      case approval: Approval =>
        addToContractIncoming(approval.forContract, approval)

      case buy: Buy =>
        addToCurrencyBuffer(buy.received.currency, buy)

      case claim: Claim =>
        val itemsToAdd = incomingByContract.getOrElse(claim.receivedFrom, ListBuffer.empty)
        itemsToAdd.addOne(claim)
        addAllToCurrencyBuffer(claim.received.currency, itemsToAdd)
        itemsToAdd.clear()

      case contrib: Contribute =>
        addToContractIncoming(contrib.to, contrib)

      case sell: Sell =>
        val entries = currencyBuffer.getOrElse(sell.sold.currency, ListBuffer.empty)
        if(entries.nonEmpty) {
          val maybeLookupContract = findFirstOccurrenceOfTokenContract(entries.toList)
          if(maybeLookupContract.isDefined && incomingByContract.contains(maybeLookupContract.get)) {
            entries.addAll(incomingByContract(maybeLookupContract.get).toList)
            incomingByContract(maybeLookupContract.get).clear()
          }
        }
        entries.addOne(sell)

        val position = Position(entries = entries.toList)
        playsBuffer.addOne(position)
        entries.clear()

      case transferIn: TransferIn =>
        addToCurrencyBuffer(transferIn.value.currency, transferIn)

      case transferOut: TransferOut =>
        addToCurrencyBuffer(transferOut.amount.currency, transferOut)
    }

    currencyBuffer.foreach {
      case (currency, entries) if entries.isEmpty => currencyBuffer.remove(currency)
      case _                                      =>
    }

    currencyBuffer.foreach {
      case (currency, transferIns) if currency == WBNB && transferIns.nonEmpty =>
        topUpsBuffer.addAll(
          transferIns.asInstanceOf[ListBuffer[TransferIn]].map(t => TopUp(t.hash, t.value, t.fee, t.timestamp))
        )
      case (_, items) if items.nonEmpty =>
        val list = items.toList
        val positionItems = findFirstOccurrenceOfTokenContract(list).flatMap(incomingByContract.get(_).map(_.toList)).getOrElse(Nil) ::: list
        playsBuffer.addOne(Position(positionItems.sortBy(_.timestamp)(Ordering[Instant])))
    }

    MarketPlays(
      items = (playsBuffer.toList ::: topUpsBuffer.toList).sortBy(_.openedAt)(Ordering[Instant])
    )
  }

  private def findFirstOccurrenceOfTokenContract(items: List[PositionEntry]): Option[WalletAddress] =
    items.head match {
      case AirDrop(receivedFrom, _, _, _, _)    => Some(receivedFrom)
      case _: Approval                          => None
      case Buy(_, _, _, coinAddress, _, _)      => Some(coinAddress)
      case Claim(_, _, receivedFrom, _, _)      => Some(receivedFrom)
      case Contribute(_, to, _, _, _)           => Some(to)
      case _: Sell                              => None
      case TransferIn(_, receivedFrom, _, _, _) => Some(receivedFrom)
      case _: TransferOut                       => None
    }
}
