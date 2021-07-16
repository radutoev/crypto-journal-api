package io.softwarechain.cryptojournal
package vo

import domain.model.NumberOfDays
import util.InstantOps

import eu.timepit.refined.refineV

import java.time.Instant

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Instant) {
  def days(): List[Instant] =
    days(start.atBeginningOfDay()).takeWhile(_.isBefore(end)).toList

  def dayCount: NumberOfDays = refineV.unsafeFrom(days().size)

  private def days(from: Instant): LazyList[Instant] =
    from #:: days(from.plusSeconds(86400))
}

object TimeInterval {
  def apply(start: Instant, end: Instant) = new TimeInterval(start, end)

  def apply(start: Instant) = new TimeInterval(start, start)
}
