package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.position.Position

final class PortfolioKpi(positions: List[Position]) {
  private lazy val closedPositions = positions.filter(_.isClosed())
  private lazy val openPositions   = positions.filter(_.isOpen())

  lazy val tradeCount: Int = closedPositions.size

  lazy val winRate: Float = {
    winRate(closedPositions)
  }

  lazy val netReturn: FungibleData = {
    closedPositions.map(_.fiatReturn()).sumFungibleData()
  }

  /**
   * Account balance derived from provided positions.
   * It takes into account open positions, and uses the total number of coins that were
   */
  lazy val balance: FungibleData = {
    openPositions.map(_.fiatValue()).sumFungibleData()
  }

  private def winRate(reference: List[Position]): Float = {
    val totalCount = reference.size
    val winCount = reference.count { position =>
      //.get is safe because win will be present on all closed positions.
      position.win().get
    }
    winCount / totalCount.toFloat
  }
}
