package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ FungibleData, TransactionHash, WBNB }

import zio.test._
import zio.test.Assertion._

object TopUpSpec extends DefaultRunnableSpec {
  override def spec = suite("TopUpSpec")(
    testM("Properties of a TopUp") {
      check(Gen.anyDouble, Gen.anyDouble, Gen.anyInstant) { (amount, fee, timestamp) =>
        val topUp = TopUp(
          TransactionHash.unsafeApply("0x03c3fd1a2e1f9dbcc2b516c7679a36c712051ac9259dec5f438d828f4c091376"),
          value = FungibleData(BigDecimal(amount), WBNB),
          fee = FungibleData(BigDecimal(fee), WBNB),
          timestamp
        )
        assert(topUp.openedAt)(equalTo(timestamp)) //&&
//          assert(topUp.totalFees())
      }
    }
  )
}
