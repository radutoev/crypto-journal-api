package io.softwarechain.cryptojournal
package domain

import domain.model.WalletAddressPredicate

import eu.timepit.refined._
import zio.test.Assertion.{ isLeft, isRight }
import zio.test._

object ModelSpec extends DefaultRunnableSpec {
  override def spec = suite("Wallet address")(
    test("Instantiate valid wallet address") {
      assert(refineV[WalletAddressPredicate]("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c"))(isRight) &&
      assert(refineV[WalletAddressPredicate]("0x10ed43c718714eb63d5aa57b78b54704e256024e"))(isRight)
    },
    test("Fail to instantiate invalid wallet address") {
      assert(refineV[WalletAddressPredicate]("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc09"))(isLeft) &&
      assert(refineV[WalletAddressPredicate]("99"))(isLeft) &&
      assert(refineV[WalletAddressPredicate]("cc10ed43c718714eb63d5aa57b78b54704e256024e"))(isLeft)
    }
  )
}
