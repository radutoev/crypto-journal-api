package io.softwarechain.cryptojournal
package domain.pricequote

import domain.currency.CurrencyRepo
import domain.model.{BUSD, CoinAddress, Currency, WBNB}
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

  override def updateQuotes(): IO[PriceQuoteError, Unit] =
    (for {
      currencies        <- currencyRepo.getCurrencies() //might not need it actually.
      latestQuotes      <- priceQuoteRepo.getLatestQuotes() //might not need it
      addressToCurrency = currencies.toMap.map(_.swap)
      startTimes = currencies.map { currencyAndAddress =>
        if (latestQuotes.contains(currencyAndAddress._1)) {
          currencyAndAddress._2 -> latestQuotes(currencyAndAddress._1).timestamp.toLocalDate()
        } else {
          currencyAndAddress._2 -> BeginningOfTime.toLocalDate()
        }
      }
      _ <- ZIO.foreachParN_(4)(startTimes) {
            case (coinAddress, start) =>
              val (quoteAddress, quoteCurrency) = quotePair(coinAddress)
              bitQueryFacade
                .getPrices(coinAddress, quoteAddress, start)
                .flatMap(quotes =>
                  priceQuoteRepo.saveQuotes(PriceQuotesChunk(addressToCurrency(coinAddress), quoteCurrency, quotes))
                )
                .ignore
          }
    } yield ()).orElseFail(PriceQuoteFetchError("Quote update failure"))

  private def quotePair(address: CoinAddress): (CoinAddress, Currency) = {
    if (!(address.value.toLowerCase == "0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c")) {
      (CoinAddress.unsafeFrom("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c"), WBNB)
    } else {
      (Currency.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56"), BUSD)
    }
  }
}

object LivePriceQuoteService {
  lazy val layer
    : URLayer[Has[PriceQuoteRepo] with Has[CurrencyRepo] with Has[BitQueryFacade] with Clock with Logging, Has[
      PriceQuoteService
    ]] =
    (LivePriceQuoteService(_, _, _, _, _)).toLayer

  private[pricequote] val BeginningOfTime = Instant.parse("2016-01-01T00:00:00.000Z")
}
