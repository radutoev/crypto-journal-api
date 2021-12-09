package io.softwarechain.cryptojournal
package vo

import domain.model.Currency
import domain.pricequote.PriceQuote

final case class PriceQuotesChunk(baseCurrency: Currency, quoteCurrency: Currency, quotes: List[PriceQuote])
