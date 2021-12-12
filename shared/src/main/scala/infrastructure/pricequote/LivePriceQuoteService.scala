package io.softwarechain.cryptojournal
package infrastructure.pricequote

import domain.pricequote.error.{PriceQuoteError, PriceQuotesSaveError}
import domain.pricequote.{CurrencyPair, PriceQuoteRepo, PriceQuoteService}
import infrastructure.bitquery.BitQueryFacade
import vo.{PriceQuotesChunk, TimeInterval}

import zio.{Has, IO, URLayer}
import zio.logging.{Logger, Logging}

import java.time.Instant

final case class LivePriceQuoteService(
  bitQueryFacade: BitQueryFacade,
  priceQuoteRepo: PriceQuoteRepo,
  logger: Logger[String]
) extends PriceQuoteService {
  override def addQuotes(pair: CurrencyPair, timestamps: List[Instant]): IO[PriceQuoteError, Unit] =
    (for {
      _      <- logger.info(s"Save price quotes for $pair")
      quotes <- bitQueryFacade.getPrices(pair, timestamps)
      _      <- priceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, quotes))
    } yield ())
      .orElseFail(PriceQuotesSaveError(pair, "Unable to save price quotes"))

  override def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, Unit] = ???
}

object LivePriceQuoteService {
  lazy val layer: URLayer[Has[BitQueryFacade] with Has[PriceQuoteRepo] with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _)).toLayer
}