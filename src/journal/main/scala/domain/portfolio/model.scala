package io.softwarechain.cryptojournal
package domain.portfolio

object model {
  sealed trait PositionPerformance
  final case object Increase extends PositionPerformance
  final case object Decrease extends PositionPerformance
  final case object NoChange extends PositionPerformance
}
