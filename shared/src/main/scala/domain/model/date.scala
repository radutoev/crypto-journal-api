package io.softwarechain.cryptojournal
package domain.model

import util.InstantOps

import java.time.Instant

object date {
  sealed trait TimeUnit
  final case object DayUnit extends TimeUnit
  final case object HourUnit extends TimeUnit
  final case object MinuteUnit extends TimeUnit


  sealed trait TimeUnitInstance {
    def value: Instant
    def unit: TimeUnit
  }


  final case class Day(value: Instant, unit: TimeUnit) extends TimeUnitInstance

  object Day {
    def apply(from: Instant): Day = {
      new Day(from.atBeginningOfDay(), DayUnit)
    }
  }

  final case class Hour(value: Instant, unit: TimeUnit) extends TimeUnitInstance {
    def toDay(): Day = Day(value)
  }

  object Hour {
    def apply(from: Instant): Hour = {
      new Hour(from.atBeginningOfHour(), HourUnit)
    }
  }

  final case class Minute(value: Instant, unit: TimeUnit) extends TimeUnitInstance

  object Minute {
    def apply(from: Instant): Minute = {
      new Minute(from.atBeginningOfMinute(), MinuteUnit)
    }
  }
}
