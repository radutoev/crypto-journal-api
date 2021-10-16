package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.portfolio.NetReturn._
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

object NetReturn {
  sealed trait PositionPerformance
  final case object Increase extends PositionPerformance
  final case object Decrease extends PositionPerformance
  final case object NoChange extends PositionPerformance
}

