package io.softwarechain.cryptojournal
package domain.pricequote

import java.time.Instant

final case class PriceQuotes(private val source: Map[CurrencyPair, List[PriceQuote]]) extends AnyVal {

  /* 4 RPL - mapped to WBNB at t0 for p0
  *  3 RPL - mapped to WBNB at t1 for p1
  *  1 WBNB - mapped to USDT at t0 for p3
  *
  * return should be something like: [ (RPL - WBNB | price), ( WBNB - USDT | price ) ]
  * */
  def findPrice(pair: CurrencyPair, timestamp: Instant): Option[PriceQuote] = {
    None
//    if(source.contains(pair)) {
//
//    } else {
//
//    }

    //


//    quotes
//      .get(currency)
//      .flatMap(currencyQuotes =>
//        currencyQuotes.filter(q => q.timestamp.isBefore(timestamp) || q.timestamp == timestamp).lastOption
//      )
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

  def apply(pair: CurrencyPair, priceQuotes: List[PriceQuote]) = new PriceQuotes(Map(pair -> priceQuotes))
}
