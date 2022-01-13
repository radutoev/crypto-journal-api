package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ FungibleDataTimePoint, Trend }
import domain.portfolio.performance.Performance

final case class PlaysOverview(
  distinctValues: PlaysDistinctValues,
  balanceTrend: Trend,
  balancePerformance: Performance,
  netReturnTrend: Trend,
  netReturnPerformance: Performance
) {
  def accountBalance: FungibleDataTimePoint = balanceTrend.latestValue
}
