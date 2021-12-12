package io.softwarechain.cryptojournal
package infrastructure.pricequote

import domain.pricequote.error.PriceQuoteError
import domain.pricequote.{CurrencyPair, PriceQuoteService}
import vo.TimeInterval

import zio.IO
import zio.logging.Logger

final case class LivePriceQuoteService (logger: Logger[String]) extends PriceQuoteService {
  override def addQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit] = {
    for {
      _ <- logger.info(s"Save price quotes for $pair in $interval")
      //fetch quotes from BitQuery
      //write to datastore
    } yield ()
  }

  override def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit] = ???
}
