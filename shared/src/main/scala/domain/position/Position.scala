package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.{PriceQuote, PriceQuotes}
import util.ListOptionOps
import vo.TimeInterval

import java.time.{Duration, Instant}

final case class Position(
  currency: Currency,
  openedAt: Instant,
  entries: List[PositionEntry],
  priceQuotes: Option[PriceQuotes] = None, //this is kind of a meta information for the aggregate.
  journal: Option[JournalEntry] = None,
  id: Option[PlayId] = None
) extends MarketPlay {
  def timeInterval(): TimeInterval = closedAt().fold(TimeInterval(openedAt))(closed => TimeInterval(openedAt, closed))

  /**
   * Total cost is calculated from all outgoing values of DEX reference currency found in the positions entries.
   * It is an absolute value.
   *
   * @return
   */
  lazy val totalCost: Option[FungibleData] = {
    totalFees.map(fees => FungibleData(outgoingSum.add(fees.amount).amount.abs, USD))
  }

  /**
   * @return Fiat sum of all fees for all entries in this position
   */
  lazy val totalFees: Option[FungibleData] = {
    priceQuotes.map(implicit quotes => entries.map(_.fee).sumFungibleData())
  }

  /**
   * Total fiat value of position.
   * Basically what is the fiat value of the coins in the position.
   *
   * TODO Do I need this?
   */
  def fiatValue(): Option[FungibleData] =
    ???
  //    priceQuotes.map { implicit quotes =>
  //      entries.collect {
  //        case e: PositionEntry if e.fiatTotal().isDefined => e.fiatValue().get
  //      }.sumFungibleData()

  /**
   * Position return derived from all position entries associated with this position.
   *
   * @return
   * None if position is not closed or no price quotes are given for the position interval.
   * FungibleData for a closed position.
   */
  lazy val fiatReturn: Option[FungibleData] = {
    if (state == Open) {
      None
    } else {
      totalCost.map(_.subtract(incomingSum.amount))
    }
  }

  /**
   * Percentage difference calculated as:
   * ((totalCost - fiatReturn) / totalCost) * 100.
   */
  def fiatReturnPercentage(): Option[BigDecimal] =
    if (state == Open) {
      None
    } else {
      for {
        totalCost  <- totalCost
        fiatReturn <- fiatReturn
      } yield util.math.percentageDiff(totalCost.amount, fiatReturn.amount + totalCost.amount)
    }

  /**
   * @return Total number of coins that were bought within this position
   */
  lazy val totalCoins: FungibleData = {
    entries.map {
      case AirDrop(_, _, received, _, _) => Some(received)
      case _: Approval                   => None
      case Buy(_, _, received, _, _, _)  => Some(received)
      case Claim(_, received, _, _, _)   => Some(received)
      case _: Contribute                 => None

      //a position cannot have TransferIns nor TransferOuts
      case _: TransferIn  => None
      case _: TransferOut => None
    }.values.sumFungibleData()
  }

  /* Alias with totalCoins */
  lazy val orderSize: FungibleData = totalCoins

  /**
   * @return Order size if only one buy or order size divided by number of buys if multiple buys in position.
   */
  def averageOrderSize(): FungibleData =
    //TODO Implement this.
    ???
//    val nrOfBuys = entries.count(_.isBuy())
//    if (nrOfBuys > 0) {
//      orderSize().divide(nrOfBuys)
//    } else {
//      FungibleData.zero(currency)
//    }

  /**
   * @return Entry coin fiat price
   */
  def entryPrice(): Option[PriceQuote] =
    priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.head.timestamp))

  /**
   * @return Exit coin fiat price
   */
  def exitPrice(): Option[PriceQuote] =
    if (closedAt().isDefined) {
      priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.last.timestamp))
    } else {
      None
    }

  def numberOfExecutions(): Int = entries.size

  def numberOfCoins(): BigDecimal = totalCoins.amount

  def holdTime(): Option[Long] = closedAt().map(closeTime => Duration.between(openedAt, closeTime).toSeconds)

  def isWin(): Option[Boolean] = fiatReturn.map(_.amount.compareTo(BigDecimal(0)) > 0)

  def isLoss(): Option[Boolean] = isWin().map(b => !b)

  //TODO Implement this
  def state: State = Open
//    entries.lastOption.fold[State](Open)(last => if (last.isSell()) Closed else Open)

  def isClosed(): Boolean = state == Closed

  def isOpen(): Boolean = state == Open

  def closedAt(): Option[Instant] = entries.lastOption.map(_.timestamp)
  //    .collect {
  //    case entry if entry.`type` == Sell => entry.timestamp
  //  }

  def inInterval(interval: TimeInterval): Boolean = {
    val startOk = interval.start.isBefore(openedAt) || interval.start == openedAt
    closedAt().fold(startOk)(t => startOk && (interval.end.isAfter(t) || interval.end == t))
  }

  lazy val outgoingSum: FungibleData = {
    priceQuotes.map { quotes =>
      entries.map {
        case _: AirDrop  => None
        case _: Approval => Some(FungibleData(0, WBNB))
        case Buy(_, spent, _, _, _, timestamp) =>
          quotes.findPrice(timestamp).map(quote => spent.amount * quote.price).map(FungibleData(_, USD))
        case _: Claim => Some(FungibleData(0, WBNB))
        case Contribute(spent, _, _, timestamp) =>
          quotes.findPrice(timestamp).map(quote => spent.amount * quote.price).map(FungibleData(_, USD))

        //a position cannot have TransferIns nor TransferOuts
        case _: TransferIn  => None
        case _: TransferOut => None
      }.values.sumFungibleData()
    }.getOrElse(FungibleData.zero(USD))
  }

  lazy val incomingSum: FungibleData = {
    priceQuotes.map { quotes =>
      entries.map {
        case _: AirDrop  => None
        case _: Approval => None
        //a position cannot have TransferIns nor TransferOuts
        case _: TransferIn  => None
        case _: TransferOut => None
        case _              => Some(FungibleData.zero(USD))
      }.values.sumFungibleData()
    }.getOrElse(FungibleData.zero(USD))
  }

//object Position {
//  type PositionEntryIdPredicate = NonEmpty
//  type PositionEntryId          = String Refined PositionEntryIdPredicate
//}

//final case class PositionEntry(
//  `type`: TransactionType,
//  value: FungibleData,
//  fee: Fee,
//  timestamp: Instant,
//  txHash: TransactionHash,
//  id: Option[PositionEntryId] = None
//) {
//  def isBuy(): Boolean = `type` == Buy
//
//  def isSell(): Boolean = false//`type` == Sell
//
//  def fiatValue()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
//    priceQuotes
//      .findPrice(timestamp)
//      .map(priceQuote => value.amount * priceQuote.price)
//      .map(fiatAmount => FungibleData(fiatAmount, refined.refineV[NonEmpty].unsafeFrom("USD")))
//
//  def fiatFee()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
//    priceQuotes
//      .findPrice(timestamp)
//      .map(priceQuote => fee.amount * priceQuote.price)
//      .map(fiatAmount => FungibleData(fiatAmount, refined.refineV[NonEmpty].unsafeFrom("USD")))
//
//  /**
//   * Calculates fiat value for the given price quotes based on the type of the entry.
//   * If entry is of type BUY, then it will return the negative absolute number of the fiatValue and fiatFee values.
//   * If entry is of type SELL, then it will subtract the fiatFee from the fiatValue.
//   */
//  def fiatReturnTotal()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
//    for {
//      fiatValue <- fiatValue()
//      fiatFee   <- fiatFee()
//      fiatReturn <- Some[BigDecimal](`type` match {
////                     case Buy  => -fiatValue.amount - fiatFee.amount
////                     case Sell => fiatValue.amount - fiatFee.amount
//                     case _    => 0
//                   })
//    } yield FungibleData(fiatReturn, refined.refineV[NonEmpty].unsafeFrom("USD"))
//
//  /**
//   * Contribution of this entry to the balance of the account.
//   * Idea is that when you buy coins, then those have an attached fiat value to it.
//   */
//  def fiatTotal()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
//    for {
//      fiatValue <- fiatValue()
//      fiatFee   <- fiatFee()
//      value <- Some[BigDecimal](`type` match {
////                case Buy  => fiatValue.amount - fiatFee.amount
////                case Sell => -fiatValue.amount - fiatFee.amount
//                case _    => 0
//              })
//    } yield FungibleData(value, refined.refineV[NonEmpty].unsafeFrom("USD"))
}
