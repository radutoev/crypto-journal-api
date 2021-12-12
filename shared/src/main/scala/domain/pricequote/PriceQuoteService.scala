package io.softwarechain.cryptojournal
package domain.pricequote

import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.IO

trait PriceQuoteService {
  def addQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit]

  def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit]
}
