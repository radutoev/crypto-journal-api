package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.model.fungible.{FungibleDataMapOps, FungibleDataOps, OptionalFungibleDataOps}
import domain.position.error.InvalidPosition
import domain.position.model.CoinName
import domain.pricequote.{PriceQuote, PriceQuotes}
import util.ListOps.cond
import util.ListOptionOps
import vo.TimeInterval

import java.time.{Duration, Instant}
import scala.collection.mutable

final case class Position(
  entries: List[PositionEntry],
  priceQuotes: PriceQuotes,
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
      case a: AirDrop                               => Some(a.received.currency)
      case _: Approval                              => None
      case Buy(_, _, received, _, _, _, _, _, _, _) => Some(received.currency)
      case Claim(_, received, _, _, _, _, _, _)     => Some(received.currency)
      case c: Contribute                            => Some(c.spent.currency)
      case Sell(sold, _, _, _, _, _)                => Some(sold.currency)
      case TransferIn(amount, _, _, _, _, _, _, _)  => Some(amount.currency)
      case TransferOut(amount, _, _, _, _, _)       => Some(amount.currency)
    }.values.distinct

    if (currencies.size > 1) {
      currencies.find(_ != WBNB)
    } else {
      currencies.headOption
    }
  }

  //TODO I think I need to see if I can add the coin address to all transaction types.
  lazy val coinAddress: Option[CoinAddress] = {
    entries.map {
      case _: AirDrop                                   => None
      case _: Approval                                  => None
      case Buy(_, _, _, _, coinAddress, _, _, _, _, _)  => Some(coinAddress)
      case Claim(_, _, _, _, _, _, _, _)                => None
      case _: Contribute                                => None
      case Sell(_, _, _, _, _, _)                       => None
      case TransferIn(_, _, _, _, _, _, _, coinAddress) => coinAddress
      case TransferOut(_, _, _, _, _, _)                => None
    }.collectFirst {
      case Some(address) => address
    }
  }

  lazy val coinName: Option[CoinName] = entries.map {
    case AirDrop(name, _, _, _, _, _, _, _)    => Some(name)
    case _: Approval                           => None
    case Buy(_, _, _, _, name, _, _, _, _, _)  => Some(name)
    case Claim(_, _, _, name, _, _, _, _)      => Some(name)
    case _: Contribute                         => None
    case _: Sell                               => None
    case TransferIn(_, _, _, _, _, name, _, _) => name
    case _: TransferOut                        => None
  }.collectFirst {
    case Some(coinName) => coinName
  }

  lazy val cost: Map[Currency, FungibleData] = {
    entries.flatMap {
      case _: AirDrop  => List.empty
      case _: Approval => List.empty
      case Buy(_, spent, _, _, _, _, _, timestamp, spentOriginal, _) =>
        List(spent) ++
          cond(
            priceQuotes.nonEmpty(),
            () =>
              priceQuotes
                .findPrice(spent.currency, timestamp)
                .map(quote => spent.amount * quote.price)
                .map(FungibleData(_, USD))
                .getOrElse(FungibleData.zero(USD))
          ) ++
          cond(spentOriginal.isDefined, () => spentOriginal.get)
      case _: Claim => List.empty
      case Contribute(spent, _, _, _, timestamp, _) =>
        List(spent) ++
          cond(
            priceQuotes.nonEmpty(),
            () =>
              priceQuotes
                .findPrice(spent.currency, timestamp)
                .map(quote => spent.amount * quote.price)
                .map(FungibleData(_, USD))
                .getOrElse(FungibleData.zero(USD))
          )
      case _: Sell        => List.empty
      case _: TransferIn  => List.empty
      case _: TransferOut => List.empty
    }.sumByCurrency
  }

  lazy val fees: Map[Currency, FungibleData] = {
    (for {
      currency    <- entries.headOption.map(_.fee.currency)
      currencyFee = entries.map(_.fee).sumByCurrency.getOrElse(currency, FungibleData.zero(currency))
      quotedFee = entries
        .map(e =>
          priceQuotes
            .findPrice(WBNB, e.timestamp)
            .map(quote => e.fee.amount * quote.price)
            .map(FungibleData(_, USD))
            .getOrElse(FungibleData.zero(USD))
        )
        .sumOfCurrency(USD)
    } yield Map(currency -> currencyFee, USD -> quotedFee)) getOrElse Map.empty
  }

  lazy val totalCost: Map[Currency, FungibleData] = {
    List(cost, fees).sumByCurrency
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
      } yield util.math.percentageDiff(totalCost.amount, fiatReturn.amount)
    }

  /**
   * @return Total number of coins that were bought within this position
   */
  lazy val totalCoins: FungibleData = {
    lazy val coinsByCurrency = entries.map {
      case AirDrop(_, _, _, received, _, _, _, _)   => Some(received)
      case _: Approval                              => None
      case Buy(_, _, received, _, _, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _, _, _)     => Some(received)
      case _: Contribute                            => None
      case _: Sell                                  => None
      case t: TransferIn                            => Some(t.value)
      case _: TransferOut                           => None
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
  lazy val entryPrice: Option[PriceQuote] = {
    for {
      c      <- currency
      quote  <- priceQuotes.findPrice(c, entries.head.timestamp)
    } yield quote
  }

  /**
   * @return Exit coin fiat price
   */
  lazy val exitPrice: Option[PriceQuote] = {
    if (closedAt.isDefined) {
      for {
        c      <- currency
        quote  <- priceQuotes.findPrice(c, entries.last.timestamp)
      } yield quote
    } else {
      None
    }
  }

  /**
   * Number of coins that are part of this Position
   */
  lazy val numberOfCoins: BigDecimal = {
    lazy val currentCoins = entries.map {
      case AirDrop(_, _, _, received, _, _, _, _)   => Some(received)
      case _: Approval                              => None
      case Buy(_, _, received, _, _, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _, _, _)     => Some(received)
      case _: Contribute                            => None
      case Sell(sold, _, _, _, _, _)                => Some(sold.negate())
      case t: TransferIn                            => Some(t.value)
      case TransferOut(amount, _, _, _, _, _)       => Some(amount.negate())
    }

    lazy val coinsByCurrency = currentCoins.sumByCurrency

    currency
      .map(value => coinsByCurrency.getOrElse(value, FungibleData.zero(value)))
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
    entries.map {
      case Sell(_, received, _, _, timestamp, _) =>
        priceQuotes.findPrice(received.currency, timestamp).map(quote => received.amount * quote.price).map(FungibleData(_, USD))
//          if (received.currency == WBNB) {
//            quotes.findPrice(WBNB, timestamp).map(quote => received.amount * quote.price).map(FungibleData(_, USD))
//          } else {
//            None
//          }
      case _ => None
    }.sumByCurrency.getOrElse(USD, FungibleData.zero(USD))
  }

  override def inInterval(interval: TimeInterval): Boolean = {
    val moreRecentThanStart = interval.start.isBefore(openedAt) || interval.start == openedAt
    closedAt.fold(moreRecentThanStart)(t => moreRecentThanStart && (interval.end.isAfter(t) || interval.end == t))
  }

  //hardcoded to USD for now
  def balance(): Option[FungibleData] = {
    var acc: BigDecimal = BigDecimal(0)

    entries.foreach { entry =>
      entry.balance().foreach { case (currency, amount) =>
        acc = acc + priceQuotes.findPrice(currency, entry.timestamp)
          .map(quote => quote.price * amount)
          .getOrElse(BigDecimal(0))
      }
    }

    Some(FungibleData(acc, USD))
  }
}

object Position {
  def apply(entries: List[PositionEntry]): Either[InvalidPosition, Position] =
    if (isSorted(entries.map(_.timestamp))(Ordering[Instant])) {
      Right(new Position(entries, PriceQuotes.empty()))
    } else {
      Left(InvalidPosition("Entries not in chronological order"))
    }

  def apply(entries: List[PositionEntry], id: PlayId): Position = new Position(entries, PriceQuotes.empty(), id = Some(id))

  def unsafeApply(entries: List[PositionEntry]): Position =
    new Position(entries, PriceQuotes.empty())

  def isSorted[T](seq: Seq[T])(implicit ord: Ordering[T]): Boolean = seq match {
    case Seq()  => true
    case Seq(_) => true
    case _      => seq.sliding(2).forall { case Seq(first, second) => ord.lteq(first, second) }
  }
}
