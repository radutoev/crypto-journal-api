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
  priceQuotes: Option[PriceQuotes] = None
) extends MarketPlay {

  override def openedAt: Instant = timestamp

  lazy val fees: Map[Currency, Fee] = {
    (List(fee.currency -> fee) ++
      cond(
        priceQuotes.isDefined,
        () =>
          USD -> priceQuotes.get
            .findPrice(WBNB, timestamp)
            .map(quote => quote.price * fee.amount)
            .map(FungibleData(_, USD))
            .getOrElse(FungibleData.zero(USD))
      )).toMap
  }
}
