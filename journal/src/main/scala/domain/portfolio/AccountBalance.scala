package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{BUSD, WBNB}
import domain.model.date.DayUnit
import domain.position._
import domain.pricequote.{CurrencyPair, PriceQuoteService}
import vo.TimeInterval

import zio.{IO, ZIO}
import zio.logging.Logger

trait AccountBalance {
  def trend(marketPlays: MarketPlays, interval: TimeInterval): IO[Throwable, List[FungibleDataTimePoint]]
}

final case class LiveAccountBalance(priceQuoteService: PriceQuoteService,
                                    logger: Logger[String]) extends AccountBalance {
  //TODO Do i need enriched plays, or not?
  //I might need to pass in the time interval here such that I get all days for this.
  override def trend(marketPlays: MarketPlays,
                     interval: TimeInterval): IO[Throwable, List[FungibleDataTimePoint]] = {
    //for all market plays - how many coins of all currencies i had for every day in the interval;
    // maybe I can merge the functionality from MarketPlays if I provide the quotes as param.

    val computeTrendEffect = for {
      bnbUsdQuotes   <- priceQuoteService.getQuotes(CurrencyPair(WBNB, BUSD), interval, DayUnit)
      coinsBnbQuotes <- priceQuoteService.getQuotes(marketPlays.currencies.map(_._1), WBNB, interval, DayUnit)
    } yield List.empty

    computeTrendEffect.orElseFail(new RuntimeException("Unable to compute account balance"))
  }
}

//final case class AccountBalance(marketPlays: MarketPlays) {
//  lazy val value = marketPlays.balanceTrend().lastOption.map(_._2).getOrElse(FungibleData.zero(BUSD))
//
//  lazy val trend = marketPlays.balanceTrend().map(_._2)
//
//  def performance(relativeTo: AccountBalance): Performance =
//    value.compare(relativeTo.value) match {
//      case Left(_) => NoChangeInPerformance
//      case Right(comparisonResult) =>
//        comparisonResult match {
//          case Equal => NoChangeInPerformance
//          case Bigger =>
//            Performance(
//              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
//              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
//              trend = Increase
//            )
//          case Lower =>
//            Performance(
//              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
//              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
//              trend = Decrease
//            )
//        }
//    }
//}


