package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.model.FungibleData.{ Bigger, Equal, Lower }
import domain.portfolio.model.{ Decrease, Increase, NoChange, PositionPerformance }
import domain.position.Positions

case class AccountBalance(positions: Positions) {
  lazy val value: FungibleData = positions.items.map(_.fiatValue()).sumFungibleData()

  lazy val trend: List[FungibleData] = positions.trend(_.fiatValue())

  def performance(relativeTo: AccountBalance): PositionPerformance = value.compare(relativeTo.value) match {
    case Left(_) => NoChange
    case Right(comparisonResult) =>
      comparisonResult match {
        case Equal  => NoChange
        case Bigger => Increase
        case Lower  => Decrease
      }
  }
}
