package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.portfolio.model.{Decrease, Increase, Performance}
import domain.position.Positions

case class AccountBalance(positions: Positions) {
  lazy val value: FungibleData = positions.items.map(_.fiatValue()).sumFungibleData()

  lazy val trend: List[FungibleData] = positions.trend(_.fiatValue())

  def performance(relativeTo: AccountBalance): Performance =
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
