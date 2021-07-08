package io.softwarechain.cryptojournal
package vo

import util.InstantOps

import java.time.Instant

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Instant) {
  def days(): Set[Instant] =
    days(start.resetHourAndMinute()).takeWhile(_.isBefore(end)).toSet

  private def days(from: Instant): LazyList[Instant] =
    from #:: days(from.plusSeconds(86400))
}

object TimeInterval {
  def apply(start: Instant, end: Instant) = new TimeInterval(start, end)

  def apply(start: Instant) = new TimeInterval(start, start)
}
