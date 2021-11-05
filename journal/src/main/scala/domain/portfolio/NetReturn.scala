package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{ Bigger, Equal, Lower }
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model._
import domain.position.MarketPlays
import domain.position.model.HideFromStats

final case class NetReturn(marketPlays: MarketPlays) {
  lazy val value: FungibleData = marketPlays.closedPositions
    .filterNot(_.journal.exists(_.scamStrategy.exists(_ == HideFromStats)))
    .map(_.fiatReturn)
    .sumFungibleData()

  lazy val trend: List[FungibleData] = marketPlays.trend(_.fiatReturn)

  def performance(relativeTo: NetReturn): Performance =
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
