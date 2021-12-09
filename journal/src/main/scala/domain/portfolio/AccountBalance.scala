package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.model.{FungibleData, USDT}
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model.{Decrease, Increase, Performance}
import domain.position._

final case class AccountBalance(marketPlays: MarketPlays) {
  lazy val value = marketPlays.balanceTrend().lastOption.map(_._2).getOrElse(FungibleData.zero(USDT))

  lazy val trend = marketPlays.balanceTrend().map(_._2)

  def performance(relativeTo: AccountBalance): Performance = {
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
}