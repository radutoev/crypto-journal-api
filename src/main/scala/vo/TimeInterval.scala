package io.softwarechain.cryptojournal
package vo

import domain.model.NumberOfDays
import util.InstantOps

import eu.timepit.refined.refineV

import java.time.Instant
import java.time.temporal.ChronoUnit

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Instant) {
  def days(): List[Instant] =
    days(start.atBeginningOfDay()).takeWhile(_.isBefore(end)).toList

  def dayCount: NumberOfDays = refineV.unsafeFrom(days().size)

  def years(): List[Int] = {
    val startYear = start.toLocalDate().getYear
    val endYear   = end.toLocalDate().getYear
    years(startYear).takeWhile(y => y <= endYear).toList
  }

  private def days(from: Instant): LazyList[Instant] =
    from #:: days(from.plus(1, ChronoUnit.DAYS))

  private def years(from: Int): LazyList[Int] =
    from #:: years(from + 1)
}

object TimeInterval {
  def apply(start: Instant, end: Instant) = new TimeInterval(start, end)

  def apply(start: Instant) = new TimeInterval(start, start)
}
