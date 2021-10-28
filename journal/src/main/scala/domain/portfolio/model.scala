package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ FungibleData, TradeCount }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

object model {
  sealed trait PositionPerformance
  final case object Increase extends PositionPerformance
  final case object Decrease extends PositionPerformance
  final case object NoChange extends PositionPerformance

  final case class Performance(absolute: BigDecimal, percentage: BigDecimal, trend: PositionPerformance)

  /**
   * Container for a FungibleData mapped to its performance relative to a comparison.
   *
   * @param performance None if there is no difference or data to compare against it.
   */
  final case class FungibleDataPerformance(data: FungibleData, performance: Option[Performance])

  object Performance {
    val NoChangeInPerformance: Performance = Performance(BigDecimal(0), BigDecimal(0), NoChange)
  }

  final case class DailyTradeData(netReturn: NetReturn, tradeCount: TradeCount)

//  type DayPredicate = MatchesRegex[W.`"""^\d{4,5}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"""`.T]
  type DayPredicate = NonEmpty
  type DayFormat    = String Refined DayPredicate
}
