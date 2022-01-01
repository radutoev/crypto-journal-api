package io.softwarechain.cryptojournal
package domain.model

import util.InstantOps

import java.time.Instant

object date {
  final case class Hour(value: Instant) extends AnyVal {
    def toDay(): Day = Day(value)
  }

  object Hour {
    def apply(from: Instant): Hour = {
      new Hour(from.atBeginningOfHour())
    }
  }

  final case class Day(value: Instant) extends AnyVal

  object Day {
    def apply(from: Instant): Day = {
      new Day(from.atBeginningOfDay())
    }
  }
}
