package io.softwarechain.cryptojournal
package domain.pricequote

import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import io.softwarechain.cryptojournal.domain.model.date.Hour
import zio.IO

import java.time.Instant

trait PriceQuoteService {
  def addQuote(pair: CurrencyAddressPair, hour: Hour): IO[PriceQuoteError, Unit]

  def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit]
}
