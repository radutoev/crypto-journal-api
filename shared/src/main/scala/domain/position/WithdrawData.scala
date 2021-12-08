package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.{CurrencyPair, PriceQuotes}
import util.ListOps.cond

import java.time.Instant

trait WithdrawData extends MarketPlayData {
  def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee]

  def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] //hardcoded to USD for now.
}

final case class WithdrawDataValues(fees: Map[Currency, Fee],
                                 balance: Option[FungibleData]) extends WithdrawData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] = fees

  override def balance(fee: Fee, value: Fee, timestamp: Instant): Option[Fee] = balance
}

final case class PriceQuoteWithdrawData(priceQuotes: PriceQuotes) extends WithdrawData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] = {
    (List(fee.currency -> fee) ++
      cond(
        priceQuotes.nonEmpty(),
        () =>
          USDT -> priceQuotes
            .findPrice(CurrencyPair(WBNB, USDT), timestamp)
            .map(quote => quote.price * fee.amount)
            .map(FungibleData(_, USDT))
            .getOrElse(FungibleData.zero(USDT))
      )).toMap
  }

  override def balance(fee: Fee, value: Fee, timestamp: Instant): Option[Fee] = {
    for {
      feeQuote   <- priceQuotes.findPrice(CurrencyPair(fee.currency, USDT), timestamp)
      valueQuote <- priceQuotes.findPrice(CurrencyPair(value.currency, USDT), timestamp)
      usdFee     = feeQuote.price * fee.amount
      usdValue   = valueQuote.price * value.amount
    } yield FungibleData(usdValue - usdFee, USDT)
  }
}
