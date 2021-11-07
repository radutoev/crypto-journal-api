package io.softwarechain.cryptojournal
package domain.pricequote

import vo.TimeInterval

import io.softwarechain.cryptojournal.domain.model.{Currency, WalletAddress}
import io.softwarechain.cryptojournal.domain.pricequote.error.PriceQuoteError
import zio.{Has, IO, Task, ZIO}

trait PriceQuoteRepo {
  //We currently support only BNB to USD price quotes.
  def getQuotes(interval: TimeInterval): Task[List[PriceQuote]]

  def getCurrentQuote(contract: WalletAddress): IO[PriceQuoteError, PriceQuote]
}

object PriceQuoteRepo {
  def getQuotes(interval: TimeInterval): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(interval))
}
