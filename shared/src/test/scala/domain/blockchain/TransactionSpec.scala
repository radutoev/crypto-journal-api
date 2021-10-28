package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{Buy, Claim, Contribute, Currency, FungibleData, Sell, TransferIn, Unknown}
import infrastructure.covalent.dto.Transaction

import zio.json._
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object TransactionSpec extends DefaultRunnableSpec {
  private val WBNB = Currency.unsafeFrom("WBNB")

  override def spec = suite("TransactionSpec")(
    test("Interpret transaction as Buy") {
      val transaction = getTransaction("/covalent/transactions/buy.json")
      val expectedValue = FungibleData(BigDecimal("0.257964600896151696"), WBNB)

      assert(Buy)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("FOOFIGHT"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as Claim") {
      val transaction = getTransaction("/covalent/transactions/claim.json")
      val expectedValue = FungibleData(BigDecimal("9335310526620.7320"), Currency.unsafeFrom("NONO"))

      assert(Claim)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("NONO"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as Contribute") {
      val transaction = getTransaction("/covalent/transactions/contribute.json")
      val expectedValue = FungibleData(BigDecimal("0.1023923391846748"), Currency.unsafeFrom("WBNB"))

      assert(Contribute)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("WBNB"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as Sell based on Withdrawal event.") {
      val transaction = getTransaction("/covalent/transactions/sell1.json")
      val expectedValue = FungibleData(BigDecimal("0.4175743142140909320"), Currency.unsafeFrom("WBNB"))

      assert(Sell)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("FOOFIGHT"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as Sell") {
      val transaction = getTransaction("/covalent/transactions/sell_after_claim.json")
      val expectedValue = FungibleData(BigDecimal("2.1870555175766987330"), Currency.unsafeFrom("WBNB"))

      assert(Sell)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("NONO"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as TransferIn") {
      val transaction = getTransaction("/covalent/transactions/transferIn.json")
      val expectedValue = FungibleData(BigDecimal("0.001"), Currency.unsafeFrom("WBNB"))

      assert(TransferIn)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value)) &&
        assert(Some("WBNB"))(equalTo(transaction.coin))
    },

    test("Interpret transaction as Unknown") {
      val transaction = getTransaction("/covalent/transactions/unknown_busd_direct_send.json")
      assert(Unknown)(equalTo(transaction.transactionType))
    }
  )

  private def getTransaction(file: String) = {
    getTransactions(file).head
  }

  private def getTransactions(file: String) = {
    val f        = readFile(file).fromJson[List[Transaction]]
    f.right.get.map(tx => tx.toDomain())
  }

  private def readFile(src: String) = {
    val source        = Source.fromURL(getClass.getResource(src))
    val rawJsonString = source.mkString
    source.close()
    rawJsonString
  }
}
