package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait PriceQuoteService {
  def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]]

  def updateQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Unit]
}

final case class LivePriceQuoteService(priceQuoteRepo: PriceQuoteRepo,
                                       logger: Logger[String]) extends PriceQuoteService {
  override def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]] = {
    priceQuoteRepo.getQuotes(currencies, timeInterval).map(_.map { case (currency, items) => currency -> PriceQuotes(items) })
  }

  override def updateQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Unit] = {
    for {
      _ <- logger.info(s"Update price quotes for currencies: ${currencies.mkString(",")}")
      //1. fetch latest timestamp for a currency and intersect with the given list.
      //2. for each currency - fetch quotes for interval from covalent.
      //3. for each currency - write quotes to datastore
    } yield ()
  }
}

object LivePriceQuoteService {
  lazy val layer: URLayer[Has[PriceQuoteRepo] with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _)).toLayer
}