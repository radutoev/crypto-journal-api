package io.softwarechain.cryptojournal
package domain.position

import domain.model.fungible.{ FungibleDataOps, OptionalFungibleDataOps }
import domain.model.{ Currency, FungibleData, USD, WBNB }
import domain.pricequote.{ PriceQuote, PriceQuotes }
import util.ListOps.cond
import util.ListOptionOps

import java.time.Instant

sealed trait PositionData extends MarketPlayData {
  def currency(entries: List[PositionEntry]): Option[Currency]
  def cost(entries: List[PositionEntry]): Map[Currency, FungibleData]
  def fees(entries: List[PositionEntry]): Map[Currency, FungibleData]
  def entryPrice(entries: List[PositionEntry]): Option[PriceQuote]
  def exitPrice(entries: List[PositionEntry]): Option[PriceQuote]
  def fiatSellValue(entries: List[PositionEntry]): FungibleData
  def balance(entries: List[PositionEntry]): Option[FungibleData] //hardcoded to USD
  def closedAt(entries: List[PositionEntry]): Option[Instant]
}

final case class PositionDataValues(
  currency: Option[Currency],
  cost: Map[Currency, FungibleData],
  fees: Map[Currency, FungibleData],
  entryPrice: Option[PriceQuote],
  exitPrice: Option[PriceQuote],
  fiatSellValue: FungibleData,
  balance: Option[FungibleData],
  closedAt: Option[Instant]
) extends PositionData {
  override def currency(entries: List[PositionEntry]): Option[Currency] = currency

  override def cost(entries: List[PositionEntry]): Map[Currency, FungibleData] = cost

  override def fees(entries: List[PositionEntry]): Map[Currency, FungibleData] = fees

  override def entryPrice(entries: List[PositionEntry]): Option[PriceQuote] = entryPrice

  override def exitPrice(entries: List[PositionEntry]): Option[PriceQuote] = exitPrice

  override def fiatSellValue(entries: List[PositionEntry]): FungibleData = fiatSellValue

  override def balance(entries: List[PositionEntry]): Option[FungibleData] = balance

  override def closedAt(entries: List[PositionEntry]): Option[Instant] = closedAt
}

final case class PriceQuotePositionData(priceQuotes: PriceQuotes) extends PositionData {
  override def cost(entries: List[PositionEntry]): Map[Currency, FungibleData] =
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

  override def fees(entries: List[PositionEntry]): Map[Currency, FungibleData] =
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

  override def entryPrice(entries: List[PositionEntry]): Option[PriceQuote] =
    for {
      c     <- currency(entries)
      quote <- priceQuotes.findPrice(c, entries.head.timestamp)
    } yield quote

  override def exitPrice(entries: List[PositionEntry]): Option[PriceQuote] =
    if (closedAt(entries).isDefined) {
      for {
        c     <- currency(entries)
        quote <- priceQuotes.findPrice(c, entries.last.timestamp)
      } yield quote
    } else {
      None
    }

  override def fiatSellValue(entries: List[PositionEntry]): FungibleData =
    entries.map {
      case Sell(_, received, _, _, timestamp, _) =>
        priceQuotes
          .findPrice(received.currency, timestamp)
          .map(quote => received.amount * quote.price)
          .map(FungibleData(_, USD))
      //          if (received.currency == WBNB) {
      //            quotes.findPrice(WBNB, timestamp).map(quote => received.amount * quote.price).map(FungibleData(_, USD))
      //          } else {
      //            None
      //          }
      case _ => None
    }.sumByCurrency.getOrElse(USD, FungibleData.zero(USD))

  override def balance(entries: List[PositionEntry]): Option[FungibleData] = {
    var acc: BigDecimal = BigDecimal(0)

    entries.foreach { entry =>
      entry.balance().foreach {
        case (currency, amount) =>
          acc = acc + priceQuotes
            .findPrice(currency, entry.timestamp)
            .map(quote => quote.price * amount)
            .getOrElse(BigDecimal(0))
      }
    }

    Some(FungibleData(acc, USD))
  }

  override def currency(entries: List[PositionEntry]): Option[Currency] = {
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

  override def closedAt(entries: List[PositionEntry]): Option[Instant] = entries.lastOption.collect {
    case entry: Sell => entry.timestamp
  }
}
