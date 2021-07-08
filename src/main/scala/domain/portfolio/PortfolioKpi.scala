package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.position.{ Position, Positions }

final class PortfolioKpi(positions: Positions) {
  lazy val tradeCount: Int = positions.closedPositions.size

  lazy val winRate: Float = {
    winRate(positions.closedPositions)
  }

  lazy val netReturn: FungibleData = {
    positions.closedPositions.map(_.fiatReturn()).sumFungibleData()
  }

  /**
   * Account balance derived from provided positions.
   * It takes into account open positions, and uses the total number of coins that were
   *
   * TODO I think I also need to check positive balanced closed positions because I could still have coins for that position.
   *  Same for the trend series as well
   */
  lazy val balance: FungibleData = {
    positions.openPositions.map(_.fiatValue()).sumFungibleData()
  }

//  lazy val balanceTrend: List[FungibleData] = {
//
//  }

  private def winRate(reference: List[Position]): Float = {
    val totalCount = reference.size
    val winCount = reference.count { position =>
      //.get is safe because win will be present on all closed positions.
      position.win().get
    }
    winCount / totalCount.toFloat
  }
}
