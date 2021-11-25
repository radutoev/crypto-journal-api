package io.softwarechain.cryptojournal
package vo

import zio.prelude._
import zio.test.Assertion._

object filter {
  object Count extends SubtypeSmart[Int](isPositive)
  type Count = Count.Type

  final case class PlayFilter(count: Count, interval: TimeInterval)

  object PlayFilter {
    def apply(count: Int, interval: TimeInterval): Validation[String, PlayFilter] =
      Count.make(count).map(posCount => new PlayFilter(posCount, interval))
  }

  final case class KpiFilter(count: Option[Count], interval: Option[TimeInterval])

  sealed trait SortOrder
  final case object Ascending  extends SortOrder
  final case object Descending extends SortOrder
}
