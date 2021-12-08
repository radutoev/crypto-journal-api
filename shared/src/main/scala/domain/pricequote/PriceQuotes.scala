package io.softwarechain.cryptojournal
package domain.pricequote

import java.time.Instant

final case class PriceQuotes(private val source: Map[CurrencyPair, List[PriceQuote]]) extends AnyVal {

  def findPrice(pair: CurrencyPair, timestamp: Instant): Option[PriceQuote] = {
    if(source.contains(pair)) {
      source.get(pair)
        .flatMap(quotes => quotes.filter(q => q.timestamp.isBefore(timestamp) || q.timestamp == timestamp).lastOption)
    } else {
      None
    }
  }

  def isEmpty(): Boolean = {
    source.isEmpty || source.map {
      case (_, quotes) => quotes.isEmpty
    }.forall(x => x)
  }

  def nonEmpty(): Boolean = !isEmpty()
}

object PriceQuotes {
  def empty(): PriceQuotes = new PriceQuotes(Map.empty)

  def apply(quotes: Map[CurrencyPair, List[PriceQuote]]) = {
    new PriceQuotes(quotes)
  }

  def apply(pair: CurrencyPair, priceQuotes: List[PriceQuote]) = new PriceQuotes(Seq(pair -> priceQuotes).toMap)
}
