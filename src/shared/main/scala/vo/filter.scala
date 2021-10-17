package io.softwarechain.cryptojournal
package vo

import zio.prelude._
import zio.test.Assertion._

object filter {
  object Count extends SubtypeSmart[Int](isPositive)
  type Count = Count.Type

  final case class PositionFilter(count: Count, interval: TimeInterval)

  object PositionFilter {
    def apply(count: Int, interval: TimeInterval): Validation[String, PositionFilter] =
      Count.make(count).map(posCount => new PositionFilter(posCount, interval))
  }

  final case class KpiFilter(count: Count, interval: Option[TimeInterval])
}
