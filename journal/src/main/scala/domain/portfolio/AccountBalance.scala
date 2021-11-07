package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.model.{FungibleData, USD, WalletAddress}
import domain.portfolio.error.PortfolioError
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model.{Decrease, Increase, Performance}
import domain.position.{MarketPlayService, MarketPlays}

import zio.IO
import zio.logging.Logger

trait AccountBalance {
  def value(address: WalletAddress): IO[PortfolioError, FungibleData]
}

final case class LiveAccountBalance(marketPlaysService: MarketPlayService,
                                    logger: Logger[String]) extends AccountBalance {
  override def value(address: WalletAddress): IO[PortfolioError, FungibleData] = {
    for {
      _ <- logger.info(s"Computing account balance for ${address.value}")
//      _ <- marketPlaysService.
//      plays <- marketPlaysService.getPlays()
    } yield null
  }
}

//final case class

final case class AccountBalance1(marketPlays: MarketPlays) {
  //TODO Should the value for the account balance take into consideration the fees as well?
  lazy val value: FungibleData = FungibleData.zero(USD)
//    marketPlays.plays
//    .filterNot {
//      case p: Position => p.journal.exists(_.scamStrategy.exists(_ == HideFromStats))
//      case _ => false
//    }
//    .map(_.fiatValue())
//    .sumFungibleData()

  //TODO Reimplement this
  lazy val trend: List[FungibleData] = List.empty // marketPlays.trend(_.fiatValue())

  def performance(relativeTo: AccountBalance): Performance = ???
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
}
