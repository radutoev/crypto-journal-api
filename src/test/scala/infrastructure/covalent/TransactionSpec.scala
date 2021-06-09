package io.softwarechain.cryptojournal
package infrastructure.covalent

import zio.test._
import zio.test.Assertion._
import zio.json._

import scala.io.Source

object TransactionSpec extends DefaultRunnableSpec {
  def spec = suite("TransactionSpec")(
    test("Transaction gets instantiated from json") {
      val source = Source.fromURL(getClass.getResource("/covalent/buy.json"))
      val rawJsonString = source.mkString
      source.close()

      val transaction = rawJsonString.fromJson[Transaction]

      assert(transaction.isRight)(equalTo(true)) &&
      assert(transaction.right.get.hash)(equalTo("0x2cfff6271130bee9c3cca60e7de5744486ba7734beef75ff9f8845f369a350cb")) &&
      assert(transaction.right.get.transactionType)(equalTo(Buy))
    }
  )
}
