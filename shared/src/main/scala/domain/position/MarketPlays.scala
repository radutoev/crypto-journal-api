package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model.fungible.OptionalFungibleDataOps
import domain.model.{ CoinAddress, Currency, FungibleData, TransactionHash, WBNB, WalletAddress }
import util.{ InstantOps, MarketPlaysListOps }
import vo.TimeInterval
import vo.TimeInterval.orderingOfTimeInterval

import java.time.Instant
import scala.collection.immutable.ListMap
import scala.collection.{ immutable, mutable }
import scala.collection.mutable.ListBuffer

//most recent items first.
final case class MarketPlays(plays: List[MarketPlay]) {
  lazy val positions: List[Position]    = plays.positions
  lazy val transferIns: List[TopUp]     = plays.transferIns
  lazy val transferOuts: List[Withdraw] = plays.transferOuts

  lazy val closedPositions: List[Position] = positions.filter(_.isClosed)
  lazy val openPositions: List[Position]   = positions.filter(_.isOpen)

  lazy val wins: List[Position]  = closedPositions.filter(_.isWin.get)
  lazy val loses: List[Position] = closedPositions.filter(_.isLoss.get)

  lazy val interval: Option[TimeInterval] = {
    if (plays.nonEmpty) {
      Some(
        TimeInterval(
          plays.head.openedAt,
          positions.map(_.closedAt).collect { case Some(closedAt) => closedAt } match {
            case Nil  => Instant.now()
            case list => list.max
          }
        )
      )
    } else {
      None
    }
  }

  lazy val currencies: Set[(Currency, CoinAddress)] =
    (plays.flatMap { play =>
      play match {
        case p: Position =>
          p.entries.map {
            case a: AirDrop                                   => Some((a.received.currency, a.coinAddress))
            case b: Buy                                       => Some((b.received.currency, b.coinAddress))
            case c: Claim                                     => Some((c.received.currency, c.coinAddress))
            case tIn: TransferIn if tIn.coinAddress.isDefined => Some((tIn.value.currency, tIn.coinAddress.get))
            case _                                            => None
          }.collect {
            case Some(currencyData) => currencyData
          }

        case _ => List.empty
      }
    } :+ (WBNB, CoinAddress.unsafeFrom("0xbb4CdB9CBd36B01bD1cBaEBF2De08d9173bc095c"))).toSet

  def filter(currency: Currency): MarketPlays =
    MarketPlays {
      plays.filter {
        case p: Position  => p.currency.contains(currency)
        case topUp: TopUp => topUp.value.currency == currency
        case w: Withdraw  => w.value.currency == currency
        case _            => false
      }
    }

  def filterByHash(hash: TransactionHash): MarketPlays =
    MarketPlays {
      plays.filter {
        case p: Position        => p.entries.exists(_.hash == hash)
        case topUp: TopUp       => topUp.txHash == hash
        case withdraw: Withdraw => withdraw.txHash == hash
        case _                  => false
      }
    }

  def currencyDistributionOverTime(currency: Currency): List[FungibleDataTimePoint] =
    distributionByCurrency().getOrElse(currency, List.empty)

  def distributionByDay(): List[(TimeInterval, List[FungibleDataTimePoint])] =
    if (interval.isDefined) {
      var dataPoints    = distributionByCurrency().values.flatten.toList.sortBy(_.timestamp)(Ordering[Instant])
      val start         = interval.get.start.atBeginningOfDay()
      val trendInterval = TimeInterval(start, Instant.now().atEndOfDay())
      trendInterval
        .days()
        .reverse
        .map(_.atEndOfDay())
        .map { day =>
          println(day)
          val filterInterval = TimeInterval(start, day)
          dataPoints = dataPoints.filter(dataPoint => filterInterval.contains(dataPoint.timestamp))
          filterInterval -> dataPoints
        }
        .sortBy(_._1)(TimeInterval.orderByEnd)
    } else {
      List.empty
    }

  def distributionByCurrency(): Map[Currency, List[FungibleDataTimePoint]] = {
    val currencyBalance: CurrencyBalance = new CurrencyBalance(mutable.Map.empty)

    plays.foreach {
      case Position(entries, _, _, _) =>
        entries.foreach {
          case a: AirDrop =>
            currencyBalance.subtract(a.fee, a.timestamp, a.hash, "Fee")
            currencyBalance.add(a.received, a.timestamp, a.hash, "AirDrop")
          case a: Approval =>
            currencyBalance.subtract(a.fee, a.timestamp, a.hash, "Fee")
          case buy: Buy =>
            currencyBalance.subtract(buy.fee, buy.timestamp, buy.hash, "Fee")
            if (buy.spentOriginal.isDefined) {
              currencyBalance.subtract(buy.spentOriginal.get, buy.timestamp, buy.hash, "Buy")
            } else {
              currencyBalance.subtract(buy.spent, buy.timestamp, buy.hash, "Buy")
            }
            currencyBalance.add(buy.received, buy.timestamp, buy.hash, "Buy")
          case c: Claim =>
            currencyBalance.subtract(c.fee, c.timestamp, c.hash, "Fee")
            currencyBalance.add(c.received, c.timestamp, c.hash, "Claim")
          case c: Contribute =>
            currencyBalance.subtract(c.fee, c.timestamp, c.hash, "Fee")
            currencyBalance.subtract(c.spent, c.timestamp, c.hash, "Contribute")
          case s: Sell =>
            currencyBalance.subtract(s.fee, s.timestamp, s.hash, "Fee")
            currencyBalance.subtract(s.sold, s.timestamp, s.hash, "Sell")
            currencyBalance.add(s.received, s.timestamp, s.hash, "Sell")
          case tIn: TransferIn =>
            currencyBalance.subtract(tIn.fee, tIn.timestamp, tIn.hash, "Fee")
            currencyBalance.add(tIn.value, tIn.timestamp, tIn.hash, "TransferIn")
          case tOut: TransferOut =>
            currencyBalance.subtract(tOut.fee, tOut.timestamp, tOut.hash, "Fee")
            currencyBalance.subtract(tOut.amount, tOut.timestamp, tOut.hash, "TransferOut")
        }
      case TopUp(hash, value, fee, timestamp, _, _) =>
        currencyBalance.subtract(fee, timestamp, hash, "Fee")
        currencyBalance.add(value, timestamp, hash, "TopUp")
      case Withdraw(hash, value, fee, timestamp, _, _) =>
        currencyBalance.subtract(fee, timestamp, hash, "Fee")
        currencyBalance.subtract(value, timestamp, hash, "Withdraw")
    }

    currencyBalance.map.toMap
  }

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
    MarketPlays {
      plays.filter {
        case p: Position =>
          val openInInterval  = interval.contains(p.openedAt)
          val closeInInterval = p.closedAt.forall(interval.contains)
          openInInterval && closeInInterval
        case t: TopUp    => interval.contains(t.timestamp)
        case w: Withdraw => interval.contains(w.timestamp)
        case _           => false
      }
    }

  //TODO Refactor this to support topUps and withdrawals??
  def trend(of: Position => Option[FungibleData]): List[FungibleData] =
    (for {
      reference <- positions.headOption
      openedAt  = reference.openedAt
      currency  <- reference.currency
      interval  = TimeInterval(openedAt.atBeginningOfDay(), Instant.now()) //should be an implicit
    } yield interval
      .days()
      .map(day =>
        filter(TimeInterval(interval.start, day)).positions
          .map(of)
          .sumByCurrency
          .getOrElse(currency, FungibleData.zero(currency))
      ))
      .getOrElse(List.empty)
}

object MarketPlays {
  def apply(items: List[MarketPlay]): MarketPlays = new MarketPlays(items)

  def empty(): MarketPlays = MarketPlays(List.empty)

  def findMarketPlays(wallet: WalletAddress, transactions: List[Transaction]): MarketPlays = {
    val currencyBuffer: mutable.Map[Currency, ListBuffer[PositionEntry]]          = mutable.Map.empty
    val incomingByContract: mutable.Map[WalletAddress, ListBuffer[PositionEntry]] = mutable.Map.empty
    val playsBuffer: ListBuffer[MarketPlay]                                       = ListBuffer.empty
    val topUpsBuffer: ListBuffer[TopUp]                                           = ListBuffer.empty
    val withdrawBuffer: ListBuffer[Withdraw]                                      = ListBuffer.empty

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

    @inline
    def getForAddress(address: WalletAddress): Option[List[PositionEntry]] =
      incomingByContract.get(address).map(_.toList)

    def createPlayIfPreviousEventIsSell(currency: Currency): Unit = {
      val entries = currencyBuffer.getOrElse(currency, ListBuffer.empty)
      if (entries.nonEmpty && entries.last.isInstanceOf[Sell]) {
        val maybeLookupContract = findFirstOccurrenceOfTokenContract(entries.toList)
        if (maybeLookupContract.isDefined && incomingByContract.contains(maybeLookupContract.get)) {
          entries.addAll(incomingByContract(maybeLookupContract.get).toList)
          incomingByContract(maybeLookupContract.get).clear()
        }
        playsBuffer.addOne(Position.unsafeApply(entries = entries.toList.sortBy(_.timestamp)(Ordering[Instant])))
        entries.clear()
      }
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
        createPlayIfPreviousEventIsSell(airDrop.received.currency)
        addToCurrencyBuffer(airDrop.received.currency, airDrop)

      case approval: Approval =>
        addToContractIncoming(approval.forContract, approval)

      case buy: Buy =>
        createPlayIfPreviousEventIsSell(buy.received.currency)
        addToCurrencyBuffer(buy.received.currency, buy)

      case claim: Claim => //check if last is Sell.
        createPlayIfPreviousEventIsSell(claim.received.currency)
        val itemsToAdd = incomingByContract.getOrElse(claim.receivedFrom, ListBuffer.empty)
        itemsToAdd.addOne(claim)
        addAllToCurrencyBuffer(claim.received.currency, itemsToAdd)
        itemsToAdd.clear()

      case contrib: Contribute =>
        addToContractIncoming(contrib.to, contrib)

      case sell: Sell =>
        addToCurrencyBuffer(sell.sold.currency, sell)

      case transferIn: TransferIn =>
        createPlayIfPreviousEventIsSell(transferIn.value.currency)
        addToCurrencyBuffer(transferIn.value.currency, transferIn)

      case transferOut: TransferOut =>
        addToCurrencyBuffer(transferOut.amount.currency, transferOut)
    }

    currencyBuffer.foreach {
      case (currency, entries) if entries.isEmpty => currencyBuffer.remove(currency)
      case _                                      =>
    }

    currencyBuffer.foreach {
      case (currency, directTransfers) if currency == WBNB && directTransfers.nonEmpty =>
        directTransfers.foreach {
          case tIn: TransferIn   => topUpsBuffer.addOne(TopUp(tIn.hash, tIn.value, tIn.fee, tIn.timestamp))
          case tOut: TransferOut => withdrawBuffer.addOne(Withdraw(tOut.hash, tOut.amount, tOut.fee, tOut.timestamp))
          case _                 =>
        }
      case (_, items) if items.nonEmpty =>
        val list = items.toList
        val positionItems = findFirstOccurrenceOfTokenContract(list)
          .flatMap(getForAddress)
          .getOrElse(Nil) ::: list
        playsBuffer.addOne(Position.unsafeApply(positionItems.sortBy(_.timestamp)(Ordering[Instant])))
    }

    MarketPlays(
      items =
        (playsBuffer.toList ::: topUpsBuffer.toList ::: withdrawBuffer.toList).sortBy(_.openedAt)(Ordering[Instant])
    )
  }

  private def findFirstOccurrenceOfTokenContract(items: List[PositionEntry]): Option[WalletAddress] =
    //TODO: receivedFrom is considered to be a wallet address, while the rest are coin addresses;
    // in essence they are the same, but see if this can be done better
    items.head match {
      case AirDrop(_, receivedFrom, _, _, _, _, _, _)   => Some(receivedFrom)
      case _: Approval                                  => None
      case Buy(_, _, _, _, _, coinAddress, _, _, _, _)  => Some(WalletAddress.unsafeFrom(coinAddress.value))
      case Claim(_, _, _, _, coinAddress, _, _, _)      => Some(WalletAddress.unsafeFrom(coinAddress.value))
      case Contribute(_, to, _, _, _, _)                => Some(to)
      case _: Sell                                      => None
      case TransferIn(_, _, _, _, _, _, coinAddress, _) => coinAddress.map(a => WalletAddress.unsafeFrom(a.value))
      case _: TransferOut                               => None
    }
}

private class CurrencyBalance(val map: mutable.Map[Currency, List[FungibleDataTimePoint]]) {
  def add(fungibleData: FungibleData, timestamp: Instant, hash: TransactionHash, entryType: String): Unit =
    map.update(
      fungibleData.currency,
      map.getOrElse(fungibleData.currency, List.empty) :+ FungibleDataTimePoint(
        fungibleData,
        timestamp,
        FungibleDataTimePointMetadata(hash, entryType)
      )
    )

  def subtract(fungibleData: FungibleData, timestamp: Instant, hash: TransactionHash, entryType: String): Unit =
    map.update(
      fungibleData.currency,
      map.getOrElse(fungibleData.currency, List.empty) :+ FungibleDataTimePoint(
        fungibleData.negate(),
        timestamp,
        FungibleDataTimePointMetadata(hash, entryType)
      )
    )
}

final case class FungibleDataTimePoint(
  fungibleData: FungibleData,
  timestamp: Instant,
  metadata: FungibleDataTimePointMetadata
) {
  override def toString: String =
    s"${fungibleData.amount},${fungibleData.currency},${timestamp},${metadata.hash},${metadata.entryType}"
}

final case class FungibleDataTimePointMetadata(hash: TransactionHash, entryType: String)
