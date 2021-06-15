package io.softwarechain.cryptojournal
package domain.pricequote

import java.time.Instant

final case class PriceQuotes (quotes: List[PriceQuote]) extends AnyVal {
  def findPrice(timestamp: Instant): Option[PriceQuote] = None
}
