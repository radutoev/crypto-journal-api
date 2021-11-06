package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.model.fungible.{FungibleDataOps, OptionalFungibleDataOps}
import domain.position.error.InvalidPosition
import domain.pricequote.{PriceQuote, PriceQuotes}
import util.ListOps.cond
import util.ListOptionOps
import vo.TimeInterval

import java.time.{Duration, Instant}

final case class Position(
  entries: List[PositionEntry],
  priceQuotes: Option[PriceQuotes] = None, //this is kind of a meta information for the aggregate.
  journal: Option[JournalEntry] = None,
  id: Option[PlayId] = None
) extends MarketPlay {
  lazy val timeInterval: TimeInterval = closedAt.fold(TimeInterval(openedAt))(closed => TimeInterval(openedAt, closed))

  /**
   * Number of transactions that are part of this position.
   */
  lazy val numberOfExecutions: Int = entries.map(_.hash).distinct.size

  lazy val currency: Option[Currency] = {
    val currencies = entries.map {
      case a: AirDrop                         => Some(a.received.currency)
      case _: Approval                        => None
      case Buy(_, _, received, _, _, _, _, _) => Some(received.currency)
      case Claim(_, received, _, _, _, _)     => Some(received.currency)
      case _: Contribute                      => None
      case Sell(sold, _, _, _, _, _)          => Some(sold.currency)
      case TransferIn(amount, _, _, _, _, _)  => Some(amount.currency)
      case TransferOut(amount, _, _, _, _, _) => Some(amount.currency)
    }.values.distinct
    if(currencies.size > 1) {
      //Just a println atm, not sure if we need to treat this case, though it *should* be impossible to get to this point.
      //However, as this is not enforced in types, so at compile time, I added this println here.
      println(s"Found multiple currencies: ${currencies.mkString(",")} on position ${id.getOrElse("")}")
    }
    currencies.headOption
  }

  lazy val cost: Map[Currency, FungibleData] = {
    entries.flatMap {
      case _: AirDrop  => List.empty
      case _: Approval => List.empty
      case Buy(_, spent, _, _, _, timestamp, spentOriginal, _) =>
        List(spent) ++
          cond(priceQuotes.isDefined && spent.currency == WBNB, () => priceQuotes.get.findPrice(timestamp).map(quote => spent.amount * quote.price).map(FungibleData(_, USD)).getOrElse(FungibleData.zero(USD))) ++
          cond(spentOriginal.isDefined, () => spentOriginal.get)
      case _: Claim => List.empty
      case Contribute(spent, _, _, _, timestamp, _) =>
        List(spent) ++
          cond(priceQuotes.isDefined && spent.currency == WBNB, () => priceQuotes.get.findPrice(timestamp).map(quote => spent.amount * quote.price).map(FungibleData(_, USD)).getOrElse(FungibleData.zero(USD)))
      case _: Sell        => List.empty
      case _: TransferIn  => List.empty
      case _: TransferOut => List.empty
    }.sumByCurrency
  }

  lazy val fees: Map[Currency, FungibleData] = {
    (for {
      currency <- entries.headOption.map(_.fee.currency)
      currencyFee = entries.map(_.fee).sumByCurrency.getOrElse(currency, FungibleData.zero(currency))
      quotes   <- priceQuotes
      quotedFee = entries.map(e => quotes.findPrice(e.timestamp).map(quote => e.fee.amount * quote.price).map(FungibleData(_, USD)).getOrElse(FungibleData.zero(USD))).sumOfCurrency(USD)
    } yield Map(currency -> currencyFee, USD -> quotedFee)) getOrElse Map.empty
  }

  lazy val totalCost: Map[Currency, FungibleData] = {
    (cost.toList ++ fees.toList).groupBy(_._1).view.mapValues(t => t.map(_._2).sumOfCurrency(t.head._1)).toMap
  }

  /**
   * Position return derived from all position entries associated with this position.
   *
   * @return
   * None if position is not closed or no price quotes are given for the position interval.
   * FungibleData for a closed position.
   */
  lazy val fiatReturn: Option[FungibleData] = {
    if (state == Closed) {
      totalCost.get(USD).map(cost => fiatSellValue.subtract(cost.amount))
    } else {
      None
    }
  }

  /**
   * Percentage difference calculated as:
   * ((totalCost - fiatReturn) / totalCost) * 100.
   */
  lazy val fiatReturnPercentage: Option[BigDecimal] =
    if (state == Open) {
      None
    } else {
      for {
        totalCost  <- totalCost.get(USD)
        fiatReturn <- fiatReturn
      } yield util.math.percentageDiff(totalCost.amount, fiatReturn.amount + totalCost.amount)
    }

  /**
   * @return Total number of coins that were bought within this position
   */
  lazy val totalCoins: FungibleData = {
    lazy val coinsByCurrency = entries.map {
      case AirDrop(_, _, received, _, _, _)   => Some(received)
      case _: Approval                        => None
      case Buy(_, _, received, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _)     => Some(received)
      case _: Contribute                      => None
      case _: Sell                            => None
      case t: TransferIn                      => Some(t.value)
      case _: TransferOut                     => None
    }.values.sumByCurrency

    currency.flatMap(coinsByCurrency.get).getOrElse(FungibleData.zero(WBNB))
  }

  lazy val orderSize: BigDecimal = totalCoins.amount

  /**
   * @return Order size if only one buy or order size divided by number of buys if multiple buys in position.
   */
  lazy val averageOrderSize: BigDecimal = {
    val nrOfBuys = entries.count {
      case _: TransferOut | _: Sell => false
      case _                        => true
    }
    if (nrOfBuys > 0) {
      orderSize / nrOfBuys
    } else {
      BigDecimal(0)
    }
  }

  /**
   * @return Entry coin fiat price
   */
  lazy val entryPrice: Option[PriceQuote] =
    priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.head.timestamp))

  /**
   * @return Exit coin fiat price
   */
  lazy val exitPrice: Option[PriceQuote] =
    if (closedAt.isDefined) {
      priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.last.timestamp))
    } else {
      None
    }

  /**
   * Number of coins that are part of this Position
   */
  lazy val numberOfCoins: BigDecimal = {
    lazy val coinsByCurrency = entries.map {
      case AirDrop(_, _, received, _, _, _)   => Some(received)
      case _: Approval                        => None
      case Buy(_, _, received, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _)     => Some(received)
      case _: Contribute                      => None
      case Sell(sold, _, _, _, _, _)          => Some(sold.negate())
      case t: TransferIn                      => Some(t.value)
      case TransferOut(amount, _, _, _, _, _) => Some(amount.negate())
    }.sumByCurrency

    currency.map(value => coinsByCurrency.getOrElse(value, FungibleData.zero(value)))
      .map(_.amount)
      .getOrElse(BigDecimal(0))
  }

  lazy val isWin: Option[Boolean] = fiatReturn.map(_.amount.compareTo(BigDecimal(0)) > 0)

  lazy val isLoss: Option[Boolean] = isWin.map(b => !b)

  lazy val state: State =
    entries.lastOption.fold[State](Open) {
      case _: Sell => Closed
      case _       => Open
    }

  lazy val isClosed: Boolean = state == Closed

  lazy val isOpen: Boolean = state == Open

  lazy val openedAt: Instant = entries.head.timestamp

  lazy val closedAt: Option[Instant] = entries.lastOption.collect {
    case entry: Sell => entry.timestamp
  }

  lazy val holdTime: Option[Long] = closedAt.map(closeTime => Duration.between(openedAt, closeTime).toSeconds)

  private lazy val fiatSellValue: FungibleData = {
    priceQuotes.map { quotes =>
      entries.map {
        case Sell(_, received, _, _, timestamp, _) =>
          if (received.currency == WBNB) {
            quotes.findPrice(timestamp).map(quote => received.amount * quote.price).map(FungibleData(_, USD))
          } else {
            None
          }
        case _ => None
      }.sumByCurrency.getOrElse(USD, FungibleData.zero(USD))
    }.getOrElse(FungibleData.zero(USD))
  }

  def inInterval(interval: TimeInterval): Boolean = {
    val startOk = interval.start.isBefore(openedAt) || interval.start == openedAt
    closedAt.fold(startOk)(t => startOk && (interval.end.isAfter(t) || interval.end == t))
  }
}

object Position {
  def apply(entries: List[PositionEntry]): Either[InvalidPosition, Position] = {
    if(isSorted(entries.map(_.timestamp))(Ordering[Instant])) {
      Right(new Position(entries))
    } else {
      Left(InvalidPosition("Entries not in chronological order"))
    }
  }

  def unsafeApply(entries: List[PositionEntry]): Position = {
    new Position(entries)
//    if(isSorted(entries.map(_.timestamp))(Ordering[Instant])) {
//
//    } else {
//      throw new RuntimeException("Invalid position - entities not in chronological order.")
//    }
  }

  def isSorted[T](seq: Seq[T])(implicit ord: Ordering[T]): Boolean = seq match {
    case Seq()  => true
    case Seq(_) => true
    case _ => seq.sliding(2).forall { case Seq(first, second) => ord.lteq(first, second) }
  }
}