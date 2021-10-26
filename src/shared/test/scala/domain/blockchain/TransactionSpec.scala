package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{Currency, FungibleData, Sell, TransferIn, WalletAddressPredicate}
import infrastructure.covalent.dto.Transaction

import eu.timepit.refined.refineV
import zio.json._
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object TransactionSpec extends DefaultRunnableSpec {
  private val Address = refineV[WalletAddressPredicate].unsafeFrom("0x627909adab1ab107b59a22e7ddd15e5d9029bc41")

  override def spec = suite("TransactionSpec")(
    test("Interpret transaction as Sell based on Withdrawal event.") {
      val transaction = getTransaction("/covalent/transactions/sell1.json")
      val expectedValue = FungibleData(BigDecimal("0.4175743142140909320"), Currency.unsafeFrom("WBNB"))

      assert(Sell)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value))
    },

    test("Interpret transaction as Sell") {
      val transaction = getTransaction("/covalent/transactions/sell_after_claim.json")
      val expectedValue = FungibleData(BigDecimal("2.1870555175766987330"), Currency.unsafeFrom("WBNB"))

      assert(Sell)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value))
    },

    test("Interpret transaction as TransferIn") {
      val transaction = getTransaction("/covalent/transactions/transferIn.json")
      val expectedValue = FungibleData(BigDecimal("0.001"), Currency.unsafeFrom("WBNB"))

      assert(TransferIn)(equalTo(transaction.transactionType)) &&
        assert(Right(expectedValue))(equalTo(transaction.value))
    }

//    test("Interpret transactions as BUYs for given address") {
//      val file         = readFile("/covalent/buys.json").fromJson[List[Transaction]]
//      val transactions = file.right.get.map(tx => AddressTransaction(Address, tx.toDomain()))
//      assert(Set(Buy))(hasSameElements(transactions.map(_.transactionType).toSet))
//    }
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
