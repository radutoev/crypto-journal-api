package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.{ CurrencyPair, PriceQuotes }
import util.ListOps.cond

import java.time.Instant

trait WithdrawData extends MarketPlayData {
  def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee]

  def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] //hardcoded to USD for now.
}

final case class WithdrawDataValues(fees: Map[Currency, Fee], balance: Option[FungibleData]) extends WithdrawData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] = fees

  override def balance(fee: Fee, value: Fee, timestamp: Instant): Option[Fee] = balance
}

final case class PriceQuoteWithdrawData(priceQuotes: PriceQuotes) extends WithdrawData {
  override def fees(fee: Fee, timestamp: Instant): Map[Currency, Fee] =
    (List(fee.currency -> fee) ++
      cond(
        priceQuotes.nonEmpty(),
        () => USDT -> priceQuotes.quotedValue(fee, USDT, timestamp).getOrElse(FungibleData.zero(USDT))
      )).toMap

  override def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] =
    for {
      feeQuote   <- priceQuotes.quotedValue(fee, USDT, timestamp)
      valueQuote <- priceQuotes.quotedValue(value, USDT, timestamp)
    } yield valueQuote.subtract(feeQuote.amount)
}
