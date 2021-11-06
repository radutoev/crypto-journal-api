package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.PriceQuotes
import util.ListOps.cond

import java.time.Instant

final case class Withdraw(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  id: Option[PlayId] = None,
  priceQuotes: Option[PriceQuotes] = None
) extends MarketPlay {
  override def openedAt: Instant = timestamp

  lazy val fees: Map[Currency, Fee] = {
    (List(fee.currency -> fee) ++
      cond(priceQuotes.isDefined, () => USD -> priceQuotes.get.findPrice(timestamp)
        .map(quote => quote.price * fee.amount)
        .map(FungibleData(_, USD))
        .getOrElse(FungibleData.zero(USD))
      )).toMap
  }
}
