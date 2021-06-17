package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Fee, FungibleData, State, TransactionType}
import domain.pricequote.PriceQuotes
import vo.TimeInterval

import java.time.Instant

final case class Position(
  coin: String,
  state: State,
  openedAt: Instant,
  closedAt: Option[Instant],
  entries: List[PositionEntry],
  priceQuotes: Option[PriceQuotes] = None //this is kind of a meta information for the aggregate.
) {
  def timeInterval(): TimeInterval = TimeInterval(openedAt, closedAt)
}

final case class PositionEntry(`type`: TransactionType, value: FungibleData, fee: Fee, timestamp: Instant) {
  def fiatValue()(implicit priceQuotes: PriceQuotes): Option[FungibleData] = {
    priceQuotes.findPrice(timestamp)
      .map(priceQuote => value.amount * priceQuote.price)
      .map(fiatAmount => FungibleData(fiatAmount, "USD"))
  }

  def fiatFee()(implicit priceQuotes: PriceQuotes): Option[FungibleData] = {
    priceQuotes.findPrice(timestamp)
      .map(priceQuote => fee.amount * priceQuote.price)
      .map(fiatAmount => FungibleData(fiatAmount, "USD"))
  }
}
