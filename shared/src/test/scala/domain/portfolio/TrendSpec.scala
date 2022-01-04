package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{BUSD, FungibleData, FungibleDataTimePoint, Trend}
import domain.portfolio.performance.Performance.NoChangeInPerformance

import zio.prelude.NonEmptyList
import zio.test.Assertion.equalTo
import zio.test.{DefaultRunnableSpec, Gen, assert, check}

object TrendSpec extends DefaultRunnableSpec {
  override def spec = suite("TrendSpec")(
    suite("Trend performance relative to a different trend")(
      testM("No change if latest values are the same") {
        check(Gen.anyInstant) { timestamp =>
          val current =
            new Trend(NonEmptyList.single(FungibleDataTimePoint(FungibleData(BigDecimal(40), BUSD), timestamp)))
          val previous = new Trend(
            NonEmptyList.single(FungibleDataTimePoint(FungibleData(BigDecimal(40), BUSD), timestamp.minusSeconds(12)))
          )
          assert(current.performance(previous))(equalTo(NoChangeInPerformance))
        }
      }
    )
  )
}
