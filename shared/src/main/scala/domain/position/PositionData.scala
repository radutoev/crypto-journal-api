package io.softwarechain.cryptojournal
package domain.position

import domain.model.fungible.{ FungibleDataOps, OptionalFungibleDataOps }
import domain.model.{ Currency, FungibleData, BUSD, WBNB }
import domain.pricequote.{ CurrencyPair, PriceQuote, PriceQuotes }
import util.ListOps.cond

import java.time.Instant

sealed trait PositionData extends MarketPlayData {
  def cost(entries: List[PositionEntry]): Map[Currency, FungibleData]
  def fees(entries: List[PositionEntry]): Map[Currency, FungibleData]
  def entryPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote]
  def exitPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote]
  def fiatSellValue(entries: List[PositionEntry]): FungibleData
  def balance(entries: List[PositionEntry]): Option[FungibleData]
  def closedAt(entries: List[PositionEntry]): Option[Instant]
}

final case class PositionDataValues(
  cost: Map[Currency, FungibleData],
  fees: Map[Currency, FungibleData],
  entryPrice: Option[PriceQuote],
  exitPrice: Option[PriceQuote],
  fiatSellValue: FungibleData,
  balance: Option[FungibleData],
  closedAt: Option[Instant]
) extends PositionData {
  override def cost(entries: List[PositionEntry]): Map[Currency, FungibleData] = cost

  override def fees(entries: List[PositionEntry]): Map[Currency, FungibleData] = fees

  override def entryPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote] = entryPrice

  override def exitPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote] = exitPrice

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
            () => priceQuotes.quotedValue(spent, BUSD, timestamp).getOrElse(FungibleData.zero(BUSD))
          ) ++
          cond(spentOriginal.isDefined, () => spentOriginal.get)
      case _: Claim => List.empty
      case Contribute(spent, _, _, _, timestamp, _) =>
        List(spent) ++
          cond(
            priceQuotes.nonEmpty(),
            () => priceQuotes.quotedValue(spent, BUSD, timestamp).getOrElse(FungibleData.zero(BUSD))
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
        .map(e => priceQuotes.quotedValue(e.fee, BUSD, e.timestamp).getOrElse(FungibleData.zero(BUSD)))
        .sumOfCurrency(BUSD)
    } yield Map(currency -> currencyFee, BUSD -> quotedFee)) getOrElse Map.empty

  override def entryPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote] =
    priceQuotes.findPrice(CurrencyPair(currency, BUSD), entries.head.timestamp)

  override def exitPrice(currency: Currency, entries: List[PositionEntry]): Option[PriceQuote] =
    if (closedAt(entries).isDefined) {
      priceQuotes.findPrice(CurrencyPair(currency, BUSD), entries.last.timestamp)
    } else {
      None
    }

  override def fiatSellValue(entries: List[PositionEntry]): FungibleData =
    entries.map {
      case Sell(_, received, _, _, timestamp, _) =>
        priceQuotes.quotedValue(received, BUSD, timestamp)
      case _ => None
    }.sumByCurrency.getOrElse(BUSD, FungibleData.zero(BUSD))

  override def balance(entries: List[PositionEntry]): Option[FungibleData] = {
    var acc: BigDecimal = BigDecimal(0)

    entries.foreach { entry =>
      entry.balance().foreach {
        case (currency, amount) =>
          acc = acc + priceQuotes.quotedValue(FungibleData(amount, currency), BUSD, entry.timestamp).map(_.amount).getOrElse(BigDecimal(0))
      }
    }

    Some(FungibleData(acc, BUSD))
  }

  override def closedAt(entries: List[PositionEntry]): Option[Instant] = entries.lastOption.collect {
    case entry: Sell => entry.timestamp
  }
}
