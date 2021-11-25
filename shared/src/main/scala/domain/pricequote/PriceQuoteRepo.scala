package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{ CoinAddress, Currency }
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.{ Has, IO, Task, ZIO }

trait PriceQuoteRepo {
  //We currently support only BNB to USD price quotes.
  def getQuotes(interval: TimeInterval): Task[List[PriceQuote]]

  def getQuotes(currencies: Set[Currency], interval: TimeInterval): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]]

  def getLatestQuotes(currency: Set[Currency]): IO[PriceQuoteError, Map[Currency, PriceQuote]]

  def saveQuotes(quotes: Map[Currency, List[PriceQuote]]): IO[PriceQuoteError, Unit]
}

object PriceQuoteRepo {
  def getQuotes(interval: TimeInterval): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(interval))

  def getQuotes(
    currencies: Set[Currency],
    interval: TimeInterval
  ): ZIO[Has[PriceQuoteRepo], PriceQuoteError, Map[Currency, List[PriceQuote]]] =
    ZIO.serviceWith(_.getQuotes(currencies, interval))
}
