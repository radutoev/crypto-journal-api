package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model.{Decrease, Increase, Performance}
import domain.position.MarketPlays

case class AccountBalance(marketPlays: MarketPlays) {
  //TODO Should the value for the account balance take into consideration the fees as well?
  lazy val value: FungibleData = marketPlays.plays.map(_.fiatValue()).sumFungibleData()

  lazy val trend: List[FungibleData] = marketPlays.trend(_.fiatValue())

  def performance(relativeTo: AccountBalance): Performance =
    value.compare(relativeTo.value) match {
      case Left(_) => NoChangeInPerformance
      case Right(comparisonResult) =>
        comparisonResult match {
          case Equal => NoChangeInPerformance
          case Bigger =>
            Performance(
              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
              trend = Increase
            )
          case Lower =>
            Performance(
              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
              trend = Decrease
            )
        }
    }
}
