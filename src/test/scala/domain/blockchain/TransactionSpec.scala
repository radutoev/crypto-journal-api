package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{ Buy, Fee, Sell }

import zio.json._
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object TransactionSpec extends DefaultRunnableSpec {
  def spec = suite("TransactionSpec")(
    test("BUY Transaction instantiation from json") {
      val source        = Source.fromURL(getClass.getResource("/covalent/buy.json"))
      val rawJsonString = source.mkString
      source.close()

      val transaction = rawJsonString.fromJson[Transaction].right.get

      assert(transaction.hash)(equalTo("0x2cfff6271130bee9c3cca60e7de5744486ba7734beef75ff9f8845f369a350cb")) &&
      assert(transaction.transactionType)(equalTo(Buy)) &&
      assert(transaction.fee)(equalTo(Fee(BigDecimal(0.0009657750000000001), "WBNB")))
    },
    test("SELL Transaction instantiation from json") {
      val source        = Source.fromURL(getClass.getResource("/covalent/sell.json"))
      val rawJsonString = source.mkString
      source.close()

      val transaction = rawJsonString.fromJson[Transaction].right.get

      assert(transaction.hash)(equalTo("0x28c21d9ebd61aa4532e0b1097342e413a33512206d04ae42f31b2f863445660a")) &&
      assert(transaction.transactionType)(equalTo(Sell)) &&
      assert(transaction.fee)(equalTo(Fee(BigDecimal(0.00103099), "WBNB")))
    }
  )
}
