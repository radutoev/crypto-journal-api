package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.position.{Position, Positions}
import vo.TimeInterval
import util.InstantOps

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
   */
  lazy val balance: FungibleData = {
    positions.items.map(_.fiatValue()).sumFungibleData()
  }

  lazy val balanceTrend: List[FungibleData] = {
    positions.timeInterval.fold[List[FungibleData]](List.empty) { interval =>
      val start = interval.start.atBeginningOfDay()
      //for every day, generate Positions with entries matching the (start -> day) time interval
      interval.days().map { day =>
        val p = positions.filter(TimeInterval(start, day))
        val result = p.items.map(_.fiatValue()).sumFungibleData()
        result
      }
    }
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
