package io.softwarechain.cryptojournal
package domain.pricequote

import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.IO

import java.time.Instant

trait PriceQuoteService {
  def addQuotes(pair: CurrencyAddressPair, timestamps: List[Instant]): IO[PriceQuoteError, Unit]

  def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit]
}
