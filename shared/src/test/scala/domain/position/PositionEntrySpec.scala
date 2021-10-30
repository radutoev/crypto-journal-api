package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Currency, FungibleData, TransactionHash, WBNB, WalletAddress }
import infrastructure.covalent.dto.Transaction

import zio.json._
import zio.test.Assertion._
import zio.test._

import java.time.Instant
import scala.io.Source

object PositionEntrySpec extends DefaultRunnableSpec {
  private val Address = WalletAddress.unsafeFrom("0x627909adab1ab107b59a22e7ddd15e5d9029bc41")
  override def spec = suite("PositionEntrySpec")(
    test("Interpret transaction as AirDrop") {
      val transaction = getTransaction("/covalent/transactions/airDrop.json")
      val airDrop     = PositionEntry.fromTransaction(transaction, Address)
      val expected = AirDrop(
        receivedFrom = WalletAddress.unsafeFrom("0xc6af635d908529d3c00a857079d9547aeecac400"),
        fee = FungibleData(BigDecimal(0.08568792), WBNB),
        received = FungibleData(BigDecimal("1000000000.0000000000000000000"), Currency.unsafeFrom("EDOGE")),
        hash = TransactionHash.unsafeApply("0xfa4cbef915290b45d8cd85e3c1b7f9fbb3c604b8ca4cff2e49222f80ffd63d38"),
        timestamp = Instant.parse("2021-05-26T10:46:15Z")
      )
      assert(airDrop)(isRight(equalTo(expected)))
    },
    test("Interpret transaction as Approval") {
      val transaction = getTransaction("/covalent/transactions/approval.json")
      val approval    = PositionEntry.fromTransaction(transaction, Address)
      val expected = Approval(
        fee = FungibleData(BigDecimal("0.00022274000000000003"), WBNB),
        hash = TransactionHash.unsafeApply("0xdc4b8148e8d106014e9f37be3b06746e04f432a271067c7f5a4eaa6ead729899"),
        timestamp = Instant.parse("2021-05-25T02:09:01Z")
      )
      assert(approval)(isRight(equalTo(expected)))
    },
    test("Interpret transaction as Buy") {
      val transaction = getTransaction("/covalent/transactions/buy.json")
      val buy         = PositionEntry.fromTransaction(transaction, Address)
      val expected = Buy(
        spent = FungibleData(BigDecimal("0.387540872900310619"), WBNB),
        received = FungibleData(BigDecimal("207074895.873037397"), Currency.unsafeFrom("FOOFIGHT")),
        coinAddress = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
        fee = FungibleData(BigDecimal("0.00377384"), WBNB),
        hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
        timestamp = Instant.parse("2021-10-18T12:27:24Z")
      )
      assert(buy)(isRight(equalTo(expected)))
    },
    test("Interpret transaction as Claim") {
      val transaction = getTransaction("/covalent/transactions/claim.json")
      val claim       = PositionEntry.fromTransaction(transaction, Address)
      val expected = Claim(
        fee = FungibleData(BigDecimal("0.0028749400000000003"), WBNB),
        received = FungibleData(BigDecimal("9335310526620.732"), Currency.unsafeFrom("NONO")),
        receivedFrom = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
        hash = TransactionHash.unsafeApply("0x1f88b75a26cab0bba6d1c8468559ad392af82c37e45b27ce07ca98d36b59d0c5"),
        timestamp = Instant.parse("2021-10-08T09:46:25Z")
      )
      assert(claim)(isRight(equalTo(expected)))
    },
    test("Interpret transaction as Contribute") {
      val transaction = getTransaction("/covalent/transactions/contribute.json")
      val contribute  = PositionEntry.fromTransaction(transaction, Address)
      val expected = Contribute(
        spent = FungibleData(BigDecimal("0.1023923391846748000"), WBNB),
        fee = FungibleData(BigDecimal("0.0010398500000000001"), WBNB),
        hash = TransactionHash.unsafeApply("0x2bba1a27a5b3e4f96316506c41e85d882e71ae900ebd08f67cfca750de38460d"),
        timestamp = Instant.parse("2021-10-07T21:45:33Z")
      )
      assert(contribute)(isRight(equalTo(expected)))
    },
    test("Interpret transaction as TransferIn") {
      val transaction = getTransaction("/covalent/transactions/transferIn.json")
      val transferIn  = PositionEntry.fromTransaction(transaction, Address)
      val expected = TransferIn(
        amount = FungibleData(BigDecimal("0.001"), WBNB),
        fee = FungibleData(BigDecimal("0.000105"), WBNB),
        receivedFrom = WalletAddress.unsafeFrom("0x0c21496dcac5826c43747c39461e5dc9461e894a"),
        hash = TransactionHash.unsafeApply("0xb80792b60eeb04747eca64ea337ce6afcbf5e2bde350e89ed11a5f456e0fa2c8"),
        timestamp = Instant.parse("2021-05-21T10:21:02Z")
      )
      assert(transferIn)(isRight(equalTo(expected)))
    },
//    test("Interpret transaction as TransferOut") {
//      ???
//    }
  )

  private def getTransaction(file: String) =
    getTransactions(file).head

  private def getTransactions(file: String) = {
    val f = readFile(file).fromJson[List[Transaction]]
    f.right.get.map(tx => tx.toDomain())
  }

  private def readFile(src: String) = {
    val source        = Source.fromURL(getClass.getResource(src))
    val rawJsonString = source.mkString
    source.close()
    rawJsonString
  }
}
