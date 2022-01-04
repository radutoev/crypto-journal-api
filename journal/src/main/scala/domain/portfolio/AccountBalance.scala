package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.date.DayUnit
import domain.model.{BUSD, WBNB}
import domain.position._
import domain.pricequote.{CurrencyPair, PriceQuoteService}
import vo.TimeInterval

import zio.{Has, IO, URLayer}
import zio.logging.{Logger, Logging}

trait AccountBalance {
  def trend(marketPlays: MarketPlays, interval: TimeInterval): IO[Throwable, List[FungibleDataTimePoint]]
}

final case class LiveAccountBalance(priceQuoteService: PriceQuoteService,
                                    logger: Logger[String]) extends AccountBalance {
  //TODO Do i need enriched plays, or not?
  override def trend(marketPlays: MarketPlays,
                     interval: TimeInterval): IO[Throwable, List[FungibleDataTimePoint]] = {
    val computeTrendEffect = for {
      bnbUsdQuotes   <- priceQuoteService.getQuotes(CurrencyPair(WBNB, BUSD), interval, DayUnit)
      coinsBnbQuotes <- priceQuoteService.getQuotes(marketPlays.currencies.map(_._1), WBNB, interval, DayUnit)
      quotes         = bnbUsdQuotes.merge(coinsBnbQuotes)
    } yield marketPlays.balanceTrend(interval, BUSD, quotes)

    computeTrendEffect.orElseFail(new RuntimeException("Unable to compute account balance"))
  }
}

object LiveAccountBalance {
  lazy val layer: URLayer[Has[PriceQuoteService] with Logging, Has[AccountBalance]] =
    (LiveAccountBalance(_, _)).toLayer
}


