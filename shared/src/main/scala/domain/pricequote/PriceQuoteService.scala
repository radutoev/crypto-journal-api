package io.softwarechain.cryptojournal
package domain.pricequote

import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import io.softwarechain.cryptojournal.domain.model.Currency
import io.softwarechain.cryptojournal.domain.model.date.{Hour, TimeUnit}
import zio.IO

import java.time.Instant

trait PriceQuoteService {
  def addQuote(pair: CurrencyAddressPair, hour: Hour): IO[PriceQuoteError, Unit]

  def getQuotes(pair: CurrencyAddressPair,
                interval: TimeInterval,
                unit: TimeUnit): IO[PriceQuoteError, PriceQuotes]

  def getQuotes(currencies: Set[Currency],
                targetCurrency: Currency,
                interval: TimeInterval,
                unit: TimeUnit): IO[PriceQuoteError, PriceQuotes]
}
