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
        () => BUSD -> priceQuotes.quotedValue(fee, BUSD, timestamp).getOrElse(FungibleData.zero(BUSD))
      )).toMap

  override def balance(fee: Fee, value: FungibleData, timestamp: Instant): Option[FungibleData] =
    for {
      feeQuote   <- priceQuotes.quotedValue(fee, BUSD, timestamp)
      valueQuote <- priceQuotes.quotedValue(value, BUSD, timestamp)
    } yield valueQuote.subtract(feeQuote.amount)
}
