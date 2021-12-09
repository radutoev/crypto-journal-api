package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.{ CurrencyPair, PriceQuotes }
import util.ListOps.cond

import java.time.Instant

sealed trait TopUpData extends MarketPlayData {
  def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee]

  def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] //hardcoded to USD for now.
}

final case class TopUpDataValues(fees: Map[Currency, Fee], balance: Option[FungibleData]) extends TopUpData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] = fees

  override def balance(fee: Fee, value: Fee, timestamp: Instant): Option[Fee] = balance
}

final case class PriceQuoteTopUpData(priceQuotes: PriceQuotes) extends TopUpData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] =
    (List(fee.currency -> fee) ++
      cond(
        priceQuotes.nonEmpty(),
        () =>
          USD -> priceQuotes
            .findPrice(CurrencyPair(WBNB, USDT), timestamp)
            .map(quote => quote.price * fee.amount)
            .map(FungibleData(_, USD))
            .getOrElse(FungibleData.zero(USD))
      )).toMap

  override def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] =
    for {
      feeQuote   <- priceQuotes.findPrice(CurrencyPair(fee.currency, USDT), timestamp)
      valueQuote <- priceQuotes.findPrice(CurrencyPair(value.currency, USDT), timestamp)
      usdFee     = feeQuote.price * fee.amount
      usdValue   = valueQuote.price * value.amount
    } yield FungibleData(usdValue - usdFee, USD)
}
