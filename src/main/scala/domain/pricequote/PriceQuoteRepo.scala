package io.softwarechain.cryptojournal
package domain.pricequote

import vo.TimeInterval

import zio.{ Has, Task, ZIO }

trait PriceQuoteRepo {
  //We currently support only BNB to USD price quotes.
  def getQuotes(interval: TimeInterval): Task[List[PriceQuote]]
}

object PriceQuoteRepo {
  def getQuotes(interval: TimeInterval): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(interval))
}
