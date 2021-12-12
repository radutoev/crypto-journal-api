package io.softwarechain.cryptojournal
package vo

import domain.pricequote.{CurrencyPair, PriceQuote}

final case class PriceQuotesChunk(pair: CurrencyPair, quotes: List[PriceQuote])
