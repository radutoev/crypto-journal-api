package io.softwarechain.cryptojournal
package domain.pricequote

import util.InstantOps
import vo.TimeInterval

import java.time.Instant

final case class PriceQuotes(quotes: List[PriceQuote]) extends AnyVal {
  def findPrice(timestamp: Instant): Option[PriceQuote] =
    quotes.filter(quote => quote.timestamp.isBefore(timestamp) || quote.timestamp == timestamp).lastOption

  def subset(interval: TimeInterval): PriceQuotes = PriceQuotes {
    val startInstant = interval.start.resetHourAndMinute()
    val endInstant   = interval.end.resetHourAndMinute()

    quotes.filter(quote => quote.timestamp.isAfter(startInstant) || quote.timestamp == startInstant).filter { quote =>
      quote.timestamp.isBefore(endInstant) || quote.timestamp == endInstant
    }
  }
}

object PriceQuotes {
  def empty(): PriceQuotes = new PriceQuotes(List.empty)

  def apply(quotes: List[PriceQuote]) = new PriceQuotes(quotes)
}
