package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.model.Performance
import domain.position.FungibleDataTimePoint

final case class PlaysOverview (distinctValues: PlaysDistinctValues,
                                balanceTrend: Trend,
                                balancePerformance: Performance) {
  def accountBalance: FungibleDataTimePoint = balanceTrend.latestValue
}
