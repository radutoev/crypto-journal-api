package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData

object model {
  sealed trait PositionPerformance
  final case object Increase extends PositionPerformance
  final case object Decrease extends PositionPerformance
  final case object NoChange extends PositionPerformance

  final case class Performance(absolute: BigDecimal, percentage: BigDecimal)

  /**
   * Container for a FungibleData mapped to its performance relative to a comparison.
   *
   * @performance None if there is no difference or data to compare against it.
   */
  final case class FungibleDataPerformance(data: FungibleData, performance: Option[Performance])
}
