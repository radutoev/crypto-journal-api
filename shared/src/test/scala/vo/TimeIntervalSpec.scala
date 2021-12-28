package io.softwarechain.cryptojournal
package vo

import eu.timepit.refined.refineMV
import zio.test.Assertion.{ equalTo, hasSameElements }
import zio.test._

import java.time.Instant

object TimeIntervalSpec extends DefaultRunnableSpec {
  override def spec: Spec[_root_.zio.test.environment.TestEnvironment, TestFailure[Nothing], TestSuccess] =
    suite("TimeInterval")(
      test("Days in time interval") {
        val start = Instant.parse("2018-12-30T18:35:24.00Z")
        val end   = Instant.parse("2019-01-02T16:35:24.00Z")

        val expected = Set(
          Instant.parse("2018-12-30T00:00:00.00Z"),
          Instant.parse("2018-12-31T00:00:00.00Z"),
          Instant.parse("2019-01-01T00:00:00.00Z"),
          Instant.parse("2019-01-02T00:00:00.00Z")
        )

        assert(TimeInterval(start, end).days())(hasSameElements(expected))
      },
      test("Days for single day interval") {
        val start = Instant.parse("2018-12-30T18:35:24.00Z")
        val end   = Instant.parse("2018-12-30T18:39:24.00Z")
        assert(TimeInterval(start, end).days())(hasSameElements(Set(Instant.parse("2018-12-30T00:00:00.00Z"))))
      },
      test("Split interval in chunks by days") {
        val start  = Instant.parse("2018-12-20T18:35:24.00Z")
        val end    = Instant.parse("2018-12-27T18:35:24.00Z")
        val chunks = TimeInterval(start, end).dayChunks(refineMV(5))
        val expected = List(
          TimeInterval(Instant.parse("2018-12-20T00:00:00.000Z"), Instant.parse("2018-12-24T00:00:00.000Z")),
          TimeInterval(Instant.parse("2018-12-24T00:00:00.000Z"), Instant.parse("2018-12-27T00:00:00.000Z"))
        )
        assert(chunks.size)(equalTo(2)) &&
        assert(chunks)(hasSameElements(expected))
      }
    )
}
