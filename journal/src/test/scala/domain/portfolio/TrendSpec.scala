package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ BUSD, FungibleData }
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.position.FungibleDataTimePoint

import zio.prelude.NonEmptyList
import zio.test.Assertion._
import zio.test._

object TrendSpec extends DefaultRunnableSpec {
  override def spec = suite("TrendSpec")(
    suite("Trend performance relative to a different trend")(
      testM("No change if latest values are the same") {
        check(Gen.anyInstant) { timestamp =>
          val current = new Trend(NonEmptyList.single(FungibleDataTimePoint(FungibleData(BigDecimal(40), BUSD), timestamp)))
          val previous = new Trend(
            NonEmptyList.single(FungibleDataTimePoint(FungibleData(BigDecimal(40), BUSD), timestamp.minusSeconds(12)))
          )
          assert(current.performance(previous))(equalTo(NoChangeInPerformance))
        }
      }
    )
  )
}
