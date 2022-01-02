package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.model.date.Minute
import domain.pricequote.error.PriceQuoteError
import vo.{PriceQuotesChunk, TimeInterval}

import zio.IO

trait PriceQuoteRepo {
  def getQuotes(pair: CurrencyPair, minutes: Set[Minute]): IO[PriceQuoteError, List[PriceQuote]]

  def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, List[PriceQuote]]

  def getLatestQuote(currency: Currency): IO[PriceQuoteError, PriceQuote]

  def saveQuotes(quotesChunk: PriceQuotesChunk): IO[PriceQuoteError, Unit]
}
