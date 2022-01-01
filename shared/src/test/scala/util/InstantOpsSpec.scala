package io.softwarechain.cryptojournal
package util

import domain.model.date.Hour

import zio.test.Assertion._
import zio.test._

import java.time.Instant

object InstantOpsSpec extends DefaultRunnableSpec {
  override def spec = suite("InstantsOpsSpec") {
    test("Group by hour") {
      val instants = Set(
        Instant.parse("2021-10-10T12:34:00.000Z"),
        Instant.parse("2021-10-10T12:45:00.000Z"),
        Instant.parse("2021-10-10T14:27:00.000Z")
      )
      val result = instants.distributionByHour
      assert(result.keySet)(
        hasSameElementsDistinct(
          Set(Hour(Instant.parse("2021-10-10T12:00:00.000Z")), Hour(Instant.parse("2021-10-10T14:00:00.000Z")))
        )
      )
    }
  }
}
