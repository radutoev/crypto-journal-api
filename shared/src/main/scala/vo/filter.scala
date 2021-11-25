package io.softwarechain.cryptojournal
package vo

import zio.prelude._
import zio.test.Assertion._

object filter {
  object Count extends SubtypeSmart[Int](isPositive)
  type Count = Count.Type

  final case class PlayFilter(count: Count, interval: TimeInterval, skip: Option[Count])

  object PlayFilter {
    def apply(count: Int, interval: TimeInterval, skip: Option[Int]): Validation[String, PlayFilter] = {
      if(skip.isDefined) {
        Validation.validateWith(Count.make(count), Count.make(skip.get)) { (posCount, posSkip) =>
          new PlayFilter(posCount, interval, Some(posSkip))
        }
      } else {
        Count.make(count).map(posCount => new PlayFilter(posCount, interval, None))
      }
    }
  }

  final case class KpiFilter(count: Option[Count], interval: Option[TimeInterval])

  sealed trait SortOrder
  final case object Ascending  extends SortOrder
  final case object Descending extends SortOrder
}
