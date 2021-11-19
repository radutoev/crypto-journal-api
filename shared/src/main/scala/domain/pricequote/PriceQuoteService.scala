package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.pricequote.error.PriceQuoteError
import util.InstantOps
import vo.TimeInterval

import io.softwarechain.cryptojournal.domain.pricequote.LivePriceQuoteService.BeginningOfTime
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

import java.time.Instant

trait PriceQuoteService {
  def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]]

  def updateQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Unit]
}

final case class LivePriceQuoteService(priceQuoteRepo: PriceQuoteRepo,
                                       clock: Clock.Service,
                                       logger: Logger[String]) extends PriceQuoteService {
  override def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]] = {
    priceQuoteRepo.getQuotes(currencies, timeInterval).map(_.map { case (currency, items) => currency -> PriceQuotes(items) })
  }

  override def updateQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Unit] = {
    for {
      _            <- logger.info(s"Update price quotes for currencies: ${currencies.mkString(",")}")
      latestQuotes <- priceQuoteRepo.getLatestQuotes(currencies)
      today        <- clock.instant.map(_.atBeginningOfDay())
      intervals    = currencies.map { currency =>
        if(latestQuotes.contains(currency)) {
          currency -> TimeInterval(latestQuotes(currency).timestamp, today)
        } else {
          currency -> TimeInterval(BeginningOfTime, today)
        }
      }
      _            <- logger.info(s"Generated intervals: ${intervals.mkString(",")}")
      //2. for each currency - fetch quotes for interval from covalent.
      //3. for each currency - write quotes to datastore
    } yield ()
  }
}

object LivePriceQuoteService {
  lazy val layer: URLayer[Has[PriceQuoteRepo] with Clock with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _)).toLayer

  private[pricequote] val BeginningOfTime = Instant.parse("2016-01-01T00:00:00.000Z")
}