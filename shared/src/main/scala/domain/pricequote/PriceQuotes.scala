package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency

import java.time.Instant

final case class PriceQuotes(quotes: Map[Currency, List[PriceQuote]]) extends AnyVal {
  def findPrice(currency: Currency, timestamp: Instant): Option[PriceQuote] =
    quotes
      .get(currency)
      .flatMap(currencyQuotes =>
        currencyQuotes.filter(q => q.timestamp.isBefore(timestamp) || q.timestamp == timestamp).lastOption
      )

  def isEmpty(): Boolean =
    quotes.map {
      case (_, quotes) =>
        quotes.isEmpty
    }.forall(x => x)

  def nonEmpty(): Boolean = !isEmpty()
}

object PriceQuotes {
  def empty(): PriceQuotes = new PriceQuotes(Map.empty)

  def apply(quotes: Map[Currency, List[PriceQuote]]) =
    new PriceQuotes(quotes.filter { case (_, quotes) => quotes.nonEmpty })

  def apply(currency: Currency, priceQuotes: List[PriceQuote]) = new PriceQuotes(Map(currency -> priceQuotes))
}
