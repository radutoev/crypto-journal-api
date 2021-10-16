package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.portfolio.model._
import domain.position.Positions

final case class NetReturn(positions: Positions) {
  lazy val value: FungibleData = positions.closedPositions.map(_.fiatReturn()).sumFungibleData()

  lazy val trend: List[FungibleData] = positions.trend(_.fiatReturn())

  def performance(relativeTo: NetReturn): PositionPerformance = value.compare(relativeTo.value) match {
    case Left(_) => NoChange
    case Right(comparisonResult) =>
      comparisonResult match {
        case Equal  => NoChange
        case Bigger => Increase
        case Lower  => Decrease
      }
  }
}

