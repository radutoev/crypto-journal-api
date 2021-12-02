package io.softwarechain.cryptojournal
package domain.pricequote

import java.time.Instant

final case class PriceQuote(price: Double, timestamp: Instant)
