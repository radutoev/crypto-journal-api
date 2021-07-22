package io.softwarechain.cryptojournal
package vo

import zio.prelude._
import zio.test.Assertion._

object filter {
  object PositionCount extends SubtypeSmart[Int](isPositive)
  type PositionCount = PositionCount.Type

  final case class LatestPositionFilter(count: PositionCount)

  object LatestPositionFilter {
    def apply(count: Int): Validation[String, PositionCount] =
      PositionCount.make(count)
  }

  final case class PositionFilter(count: PositionCount, interval: TimeInterval)

  object PositionFilter {
    def apply(count: Int, interval: TimeInterval): Validation[String, PositionFilter] =
      PositionCount.make(count).map(posCount => new PositionFilter(posCount, interval))
  }

  final case class KpiFilter(interval: TimeInterval)
}
