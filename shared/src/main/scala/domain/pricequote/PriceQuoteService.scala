package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.IO

trait PriceQuoteService {
  def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]]
}