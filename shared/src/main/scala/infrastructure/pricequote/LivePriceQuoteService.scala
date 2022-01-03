package io.softwarechain.cryptojournal
package infrastructure.pricequote

import domain.model.Currency
import domain.model.date.{Hour, TimeUnit}
import domain.pricequote.error.{PriceQuoteError, PriceQuotesSaveError}
import domain.pricequote._
import infrastructure.bitquery.BitQueryFacade
import vo.{PriceQuotesChunk, TimeInterval}

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

final case class LivePriceQuoteService(
  bitQueryFacade: BitQueryFacade,
  priceQuoteRepo: PriceQuoteRepo,
  logger: Logger[String]
) extends PriceQuoteService {

  override def addQuote(pair: CurrencyAddressPair, hour: Hour): IO[PriceQuoteError, Unit] = {
    (for {
      _      <- logger.info(s"Save price quotes ${pair.base.currency} -> ${pair.quote.currency} @ ${hour.value}")
      quotes <- bitQueryFacade.getPrices(pair, Set(hour.value))
      cPair  = CurrencyPair(pair.base.currency, pair.quote.currency)
      _      <- priceQuoteRepo.saveQuotes(PriceQuotesChunk(cPair, quotes))
    } yield ()).orElseFail(PriceQuotesSaveError(CurrencyPair(pair.base.currency, pair.quote.currency), "Unable to save price quotes"))
  }

  override def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit] = {
    ???
  }

  override def getQuotes(quote: Currency, interval: TimeInterval, unit: TimeUnit): IO[PriceQuoteError, List[PriceQuote]] = ???
}

object LivePriceQuoteService {
  lazy val layer: URLayer[Has[BitQueryFacade] with Has[PriceQuoteRepo] with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _)).toLayer
}