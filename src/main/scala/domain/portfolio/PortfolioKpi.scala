package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.position.{Position, Positions}
import util.InstantOps
import vo.TimeInterval

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV

import java.time.Instant

/**
 * @param positions used as data source for KPI calculations
 * @param interval given time interval for KPI calculations.
 */
final class PortfolioKpi(positions: Positions, interval: TimeInterval) {
  lazy val tradeCount: Int = positions.closedPositions.size

  lazy val openTradesCount: Int = positions.openPositions.size

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
    positions.items.headOption.map(_.openedAt).fold[List[FungibleData]](List.empty) { openedAt =>
      val interval = TimeInterval(openedAt.atBeginningOfDay(), Instant.now()) //should be an implicit
      interval.days().map { day =>
        positions.filter(TimeInterval(interval.start, day)).items.map(_.fiatValue()).sumFungibleData()
      }
    }
  }

  /**
   * Sum all fees of positions
   */
  lazy val totalFees: FungibleData = {
    positions.items.map(_.totalFees()).collect {
      case Some(fee) => fee
    }.sumFungibleData()
  }

  /**
   * Uses the given time interval to derive the average trade count.
   */
  lazy val avgDailyTradeCount: Float = {
    positions.closedPositions.size.toFloat / interval.dayCount.value
  }

  lazy val totalWins: Int Refined NonNegative = {
    refineV.unsafeFrom(positions.closedPositions.count(_.win().get))
  }

  lazy val totalLoses: Int Refined NonNegative = {
    refineV.unsafeFrom(positions.closedPositions.size - totalWins.value)
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
