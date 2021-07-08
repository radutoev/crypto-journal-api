package io.softwarechain.cryptojournal
package vo

import java.time.Instant

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Instant) {
//  def days():
}

object TimeInterval {
  def apply(start: Instant, end: Instant) = new TimeInterval(start, end)

  def apply(start: Instant) = new TimeInterval(start, start)
}
