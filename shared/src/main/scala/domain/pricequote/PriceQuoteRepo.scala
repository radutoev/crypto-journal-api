package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{ CoinAddress, Currency }
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.{ Has, IO, Task, ZIO }

trait PriceQuoteRepo {
  def getQuotes(currencies: Set[Currency], interval: TimeInterval): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]]

  def getLatestQuotes(): IO[PriceQuoteError, Map[Currency, PriceQuote]]

  def saveQuotes(quotes: Map[Currency, List[PriceQuote]]): IO[PriceQuoteError, Unit]
}

object PriceQuoteRepo {
  def getQuotes(
    currencies: Set[Currency],
    interval: TimeInterval
  ): ZIO[Has[PriceQuoteRepo], PriceQuoteError, Map[Currency, List[PriceQuote]]] =
    ZIO.serviceWith(_.getQuotes(currencies, interval))
}
