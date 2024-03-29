package io.softwarechain.cryptojournal
package vo

import domain.model.NumberOfDays
import util.InstantOps

import eu.timepit.refined.refineV
import eu.timepit.refined.types.numeric.PosInt
import io.softwarechain.cryptojournal.domain.model.date.Hour
import io.softwarechain.cryptojournal.vo.TimeInterval.SecondsInDay

import java.time.Instant
import java.time.temporal.ChronoUnit

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Instant) {
  def days(): List[Instant] =
    days(start.atBeginningOfDay()).takeWhile(_.isBefore(end)).toList

  def dayChunks(dayCount: PosInt): List[TimeInterval] = {
    val startFrom = start.atBeginningOfDay()

    days(startFrom)
      .takeWhile(_.isBefore(end))
      .grouped(dayCount.value)
      .map { lazyTimestamps =>
        val timestamps = lazyTimestamps.toList
        if(timestamps.head == startFrom) {
          TimeInterval(timestamps.head, timestamps.last)
        } else {
          TimeInterval(timestamps.head.minusSeconds(SecondsInDay), timestamps.last)
        }
      }
      .toList
  }

  def dayCount: NumberOfDays = refineV.unsafeFrom(days().size)

  def years(): List[Int] = {
    val startYear = start.toLocalDate().getYear
    val endYear   = end.toLocalDate().getYear
    years(startYear).takeWhile(y => y <= endYear).toList
  }

  def hours: List[Hour] = {
    hours(start).takeWhile(_.value.isBefore(end)).toList
  }

  /**
   * Shifts the interval to an earlier interval
   * @param days Number of days before to move the start and end values of the interval
   * @return New TimeInterval
   */
  def minus(days: NumberOfDays): TimeInterval = {
    val seconds = days.value * SecondsInDay
    TimeInterval(start.minusSeconds(seconds), end.minusSeconds(seconds))
  }

  def contains(timestamp: Instant): Boolean =
    if (timestamp == start || timestamp == end) true
    else start.isBefore(timestamp) && end.isAfter(timestamp)

  private def hours(from: Instant): LazyList[Hour] = {
    Hour(from) #:: hours(from.plus(1, ChronoUnit.HOURS))
  }

  private def days(from: Instant): LazyList[Instant] =
    from #:: days(from.plus(1, ChronoUnit.DAYS))

  private def years(from: Int): LazyList[Int] =
    from #:: years(from + 1)
}

object TimeInterval {
  private val SecondsInDay = 86400

  def apply(start: Instant, end: Instant) = new TimeInterval(start, end)

  def apply(start: Instant) = new TimeInterval(start, start)

  implicit def orderingOfTimeInterval[T <: TimeInterval]: Ordering[T] = Ordering.by(t => t.start)

  implicit def orderByEnd[T <: TimeInterval]: Ordering[T] = Ordering.by(_.end)
}
