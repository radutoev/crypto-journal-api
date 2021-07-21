package io.softwarechain.cryptojournal
package vo

import PositionFilter._

import zio.prelude._
import zio.test.Assertion._

final case class PositionFilter(count: PositionCount, interval: TimeInterval)

object PositionFilter {
  object PositionCount extends SubtypeSmart[Int](isPositive)
  type PositionCount = PositionCount.Type

  def apply(count: Int, interval: TimeInterval): Validation[String, PositionFilter] =
    PositionCount.make(count).map(posCount => new PositionFilter(posCount, interval))
}
