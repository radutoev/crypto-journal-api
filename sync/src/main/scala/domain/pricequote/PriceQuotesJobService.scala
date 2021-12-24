package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{BUSD, CoinAddress, CurrencyAddress, WBNB}
import domain.pricequote.LivePriceQuotesJobService.BeginningOfTime
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError, PriceQuoteNotFound}
import infrastructure.bitquery.BitQueryFacade
import util.InstantOps
import vo.PriceQuotesChunk

import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, UIO, URLayer}

import java.time.Instant

trait PriceQuotesJobService {
  def updateQuotes(): IO[PriceQuoteError, Unit]
}

final case class LivePriceQuotesJobService(
  priceQuoteRepo: PriceQuoteRepo,
  bitQueryFacade: BitQueryFacade,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuotesJobService {

  override def updateQuotes(): IO[PriceQuoteError, Unit] =
    (for {
      startTime <- priceQuoteRepo
                    .getLatestQuote(WBNB)
                    .tap(quote => logger.info(s"Latest quote for ${WBNB.value} on ${quote.timestamp}"))
                    .map(_.timestamp.plusSeconds(60))
                    .catchSome {
                      case PriceQuoteNotFound(_) => logger.info(s"Price quote not found for ${WBNB.value}") *> UIO(BeginningOfTime)
                    }
      _ <- logger.info(s"Update WBNB quotes from $startTime")
      _ <- bitQueryFacade
            .getPrices(
              CurrencyAddressPair(
                CurrencyAddress(
                  WBNB, CoinAddress.unsafeFrom("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c")
                ),
                CurrencyAddress(
                  BUSD,
                  CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56")
                )
              ),
              startTime.toLocalDate()
            )
            .flatMap(quotes => priceQuoteRepo.saveQuotes(PriceQuotesChunk(CurrencyPair(WBNB, BUSD), quotes)))
    } yield ()).orElseFail(PriceQuoteFetchError("Quote update failure"))

}

object LivePriceQuotesJobService {
  lazy val layer
    : URLayer[Has[PriceQuoteRepo] with Has[BitQueryFacade] with Clock with Logging, Has[
      PriceQuotesJobService
    ]] =
    (LivePriceQuotesJobService(_, _, _, _)).toLayer

  private[pricequote] val BeginningOfTime = Instant.parse("2021-04-01T00:00:00.000Z")
}
