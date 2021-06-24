package io.softwarechain.cryptojournal
package domain.portfolio

import domain.position.Position

//I have the positions, so I can hardcode the timeline.
final class PortfolioKpi (positions: List[Position]) {
  lazy val tradeCount = positions.count(_.isClosed())

  lazy val winRate: Float = {
    val closedPositions = positions.filter(_.isClosed())
    winRate(closedPositions)
  }

  private def winRate(reference: List[Position]): Float = {
    val totalCount = reference.size
    val winCount = reference.count(position => {
      //.get is safe because win will be present on all closed positions.
      position.win().get
    })
    winCount / totalCount
  }
}
