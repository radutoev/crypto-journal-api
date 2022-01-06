package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.model.date.{Minute, TimeUnit}
import domain.pricequote.error.PriceQuoteError
import vo.{PriceQuotesChunk, TimeInterval}

import zio.{Has, IO, ZIO}

trait PriceQuoteRepo {
  def getQuotes(pair: CurrencyPair, minutes: Set[Minute]): IO[PriceQuoteError, List[PriceQuote]]

  def getQuotes(pair: CurrencyPair, interval: TimeInterval, unit: TimeUnit): IO[PriceQuoteError, List[PriceQuote]]

  def getQuotes(currencies: Set[Currency], targetCurrency: Currency, interval: TimeInterval, unit: TimeUnit): IO[PriceQuoteError, List[(CurrencyPair, PriceQuote)]]

  def getLatestQuote(currency: Currency): IO[PriceQuoteError, PriceQuote]

  def saveQuotes(quotesChunk: PriceQuotesChunk): IO[PriceQuoteError, Unit]
}

object PriceQuoteRepo {
  def getQuotes(pair: CurrencyPair, interval: TimeInterval, unit: TimeUnit): ZIO[Has[PriceQuoteRepo], PriceQuoteError, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(pair, interval, unit))

  def saveQuotes(quotesChunk: PriceQuotesChunk): ZIO[Has[PriceQuoteRepo], PriceQuoteError, Unit] =
    ZIO.serviceWith(_.saveQuotes(quotesChunk))
}