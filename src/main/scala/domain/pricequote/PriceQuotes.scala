package io.softwarechain.cryptojournal
package domain.pricequote

import vo.TimeInterval

import java.time.Instant

final case class PriceQuotes(quotes: List[PriceQuote]) extends AnyVal {
  def findPrice(timestamp: Instant): Option[PriceQuote] =
    quotes.filter(quote => quote.timestamp.isBefore(timestamp) || quote.timestamp == timestamp ).lastOption

  def subset(interval: TimeInterval): PriceQuotes = PriceQuotes {
    quotes.filter(quote => quote.timestamp.isAfter(interval.start) || quote.timestamp == interval.start)
      .filter { quote =>
        if(interval.end.isDefined) {
          val end = interval.end.get
          quote.timestamp.isBefore(end) || quote.timestamp == end
        } else {
          true
        }
      }
  }
}

object PriceQuotes {
  def empty(): PriceQuotes = new PriceQuotes(List.empty)

  def apply(quotes: List[PriceQuote]) = new PriceQuotes(quotes)
}
