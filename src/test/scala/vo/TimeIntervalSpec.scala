package io.softwarechain.cryptojournal
package vo

import zio.test._
import zio.test.Assertion._

import java.time.Instant

object TimeIntervalSpec extends DefaultRunnableSpec {
  override def spec = suite("TimeInterval")(
    test("days in time interval") {
      val start = Instant.parse("2018-12-30T18:35:24.00Z")
      val end   = Instant.parse("2019-01-02T16:35:24.00Z")
      TimeInterval(start, end)
    }
  )
}
