package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Currency, FungibleData, WalletAddress}
import infrastructure.covalent.dto.Transaction

import zio.json._
import zio.test.Assertion._
import zio.test._

import java.time.Instant
import scala.io.Source

object PositionEntrySpec extends DefaultRunnableSpec {
  private val WBNB = Currency.unsafeFrom("WBNB")

  override def spec = suite("PositionEntrySpec")(
    test("Interpret transaction as Buy") {
      val transaction = getTransaction("/covalent/transactions/buy.json")
      val buy = NewPositionEntry.fromTransaction(transaction)
      val expected = Buy(
        spent = FungibleData(BigDecimal("0.387540872900310619"), WBNB),
        received = FungibleData(BigDecimal("207074895.873037397"), Currency.unsafeFrom("FOOFIGHT")),
        coinAddress = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
        fee = FungibleData(BigDecimal("0.00377384"), WBNB),
        hash = transaction.hash,
        timestamp = Instant.parse("2021-10-18T12:27:24Z")
      )
      assert(buy)(isRight(equalTo(expected)))
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
