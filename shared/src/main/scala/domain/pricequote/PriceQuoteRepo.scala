package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.pricequote.error.PriceQuoteError
import vo.{PriceQuotesChunk, TimeInterval}

import zio.{Has, IO, ZIO}

trait PriceQuoteRepo {
  def getQuotes(interval: TimeInterval): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]]

  def getQuotes(currency: Currency, interval: TimeInterval): IO[PriceQuoteError, List[PriceQuote]]

  def getLatestQuotes(): IO[PriceQuoteError, Map[Currency, PriceQuote]]

  def saveQuotes(quotesChunk: PriceQuotesChunk): IO[PriceQuoteError, Unit]
}

object PriceQuoteRepo {
  def getQuotes(
    interval: TimeInterval
  ): ZIO[Has[PriceQuoteRepo], PriceQuoteError, Map[Currency, List[PriceQuote]]] =
    ZIO.serviceWith(_.getQuotes(interval))
}
