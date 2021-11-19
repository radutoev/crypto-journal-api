package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.CoinAddress
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.{Has, IO, Task, ZIO}

trait PriceQuoteRepo {
  //We currently support only BNB to USD price quotes.
  def getQuotes(interval: TimeInterval): Task[List[PriceQuote]]

  def getCurrentQuote(contract: CoinAddress): IO[PriceQuoteError, PriceQuote]
}

object PriceQuoteRepo {
  def getQuotes(interval: TimeInterval): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(interval))
}
