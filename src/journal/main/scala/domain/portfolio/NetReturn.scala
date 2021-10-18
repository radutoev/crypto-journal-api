package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{ Bigger, Equal, Lower }
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model._
import domain.position.Positions

final case class NetReturn(positions: Positions) {
  lazy val value: FungibleData = positions.closedPositions.map(_.fiatReturn()).sumFungibleData()

  lazy val trend: List[FungibleData] = positions.trend(_.fiatReturn())

  def performance(relativeTo: NetReturn): Performance =
    value.compare(relativeTo.value) match {
      case Left(_) => NoChangeInPerformance
      case Right(comparisonResult) =>
        comparisonResult match {
          case Equal => NoChangeInPerformance
          case Bigger =>
            Performance(
              absolute = value.difference(relativeTo.value).right.get,
              percentage = value.percentageDifference(relativeTo.value).right.get,
              trend = Increase
            )
          case Lower =>
            Performance(
              absolute = value.difference(relativeTo.value).right.get,
              percentage = value.percentageDifference(relativeTo.value).right.get,
              trend = Decrease
            )
        }
    }
}
