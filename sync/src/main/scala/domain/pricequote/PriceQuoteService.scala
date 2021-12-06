package io.softwarechain.cryptojournal
package domain.pricequote

import domain.currency.CurrencyRepo
import domain.model.{CoinAddress, Currency, WBNB}
import domain.pricequote.LivePriceQuoteService.BeginningOfTime
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError}
import infrastructure.bitquery.BitQueryFacade
import util.InstantOps
import vo.PriceQuotesChunk

import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer, ZIO}

import java.time.Instant

trait PriceQuoteService {
  def updateQuotes(): IO[PriceQuoteError, Unit]
}

final case class LivePriceQuoteService(
  priceQuoteRepo: PriceQuoteRepo,
  currencyRepo: CurrencyRepo,
  bitQueryFacade: BitQueryFacade,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteService {
  override def updateQuotes(): IO[PriceQuoteError, Unit] = {
    (for {
      currencies        <- currencyRepo.getCurrencies()
      latestQuotes      <- priceQuoteRepo.getLatestQuotes()
      addressToCurrency = currencies.toMap.map(_.swap)
      startTimes        = currencies.map { currencyAndAddress =>
        if (latestQuotes.contains(currencyAndAddress._1)) {
          currencyAndAddress._2 -> latestQuotes(currencyAndAddress._1).timestamp.toLocalDate()
        } else {
          currencyAndAddress._2 -> BeginningOfTime.toLocalDate()
        }
      }
      _ <- ZIO.foreachParN_(4)(startTimes) { case (coinAddress, start) =>
        bitQueryFacade
          .getPrices(coinAddress, CoinAddress.unsafeFrom("0xbb4CdB9CBd36B01bD1cBaEBF2De08d9173bc095c"), start)
          .flatMap(quotes => priceQuoteRepo.saveQuotes(PriceQuotesChunk(addressToCurrency(coinAddress), WBNB, quotes)))
          .ignore
      }
    } yield ()).orElseFail(PriceQuoteFetchError("Quote update failure"))
  }
}

object LivePriceQuoteService {
  lazy val layer
    : URLayer[Has[PriceQuoteRepo] with Has[CurrencyRepo] with Has[BitQueryFacade] with Clock with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _, _, _)).toLayer

  private[pricequote] val BeginningOfTime = Instant.parse("2016-01-01T00:00:00.000Z")
}
