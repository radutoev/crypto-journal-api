package io.softwarechain.cryptojournal
package domain.model

import zio.test._
import zio.test.Assertion._

object FungibleDataSpec extends DefaultRunnableSpec {
  override def spec = suite("FungibleDataSpec")(
    suite("Percentage difference")(
      test("No difference for same values") {
        val difference = FungibleData(BigDecimal(10), WBNB).percentageDifference(FungibleData(BigDecimal(10), WBNB))
        assert(difference)(isRight(equalTo(BigDecimal(0))))
      },
      test("Fail computation if different currencies") {
        assert(FungibleData(BigDecimal(10), WBNB).percentageDifference(FungibleData(BigDecimal(10), BUSD)))(isLeft)
      },
      test("Absolute difference for increase") {
        val difference = FungibleData(BigDecimal(10), WBNB).percentageDifference(FungibleData(BigDecimal(20), WBNB))
        assert(difference)(isRight(equalTo(BigDecimal(100.00))))
      },
      test("Absolute difference for decrease") {
        val difference = FungibleData(BigDecimal(10), WBNB).percentageDifference(FungibleData(BigDecimal(-20), WBNB))
        assert(difference)(isRight(equalTo(BigDecimal(300.00))))
      }
    )
  )
}
