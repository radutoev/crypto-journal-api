package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.PriceQuotes
import util.ListOps.cond

import java.time.Instant

//TODO Should I handle PriceQuotes differently?? Maybe as an implicit on the functions??
final case class TopUp(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  id: Option[PlayId] = None,
  priceQuotes: PriceQuotes
) extends MarketPlay {

  override def openedAt: Instant = timestamp

  lazy val fees: Map[Currency, Fee] = {
    (List(fee.currency -> fee) ++
      cond(
        priceQuotes.nonEmpty(),
        () =>
          USD -> priceQuotes
            .findPrice(WBNB, timestamp)
            .map(quote => quote.price * fee.amount)
            .map(FungibleData(_, USD))
            .getOrElse(FungibleData.zero(USD))
      )).toMap
  }

  //hardcoded to USD for now.
  def balance(): Option[FungibleData] =
    for {
      feeQuote   <- priceQuotes.findPrice(fee.currency, timestamp)
      valueQuote <- priceQuotes.findPrice(value.currency, timestamp)
      usdFee     = feeQuote.price * fee.amount
      usdValue   = valueQuote.price * value.amount
    } yield FungibleData(usdValue - usdFee, USD)
}

object TopUp {
  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant): TopUp =
    new TopUp(txHash, value, fee, timestamp, None, PriceQuotes.empty())

  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant, id: PlayId): TopUp =
    new TopUp(txHash, value, fee, timestamp, Some(id), PriceQuotes.empty())
}
