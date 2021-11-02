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
      assert(airDrop)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as Approval") {
      val transaction = getTransaction("/covalent/transactions/approval.json")
      val approval    = PositionEntry.fromTransaction(transaction, Address)
      val expected = Approval(
        fee = FungibleData(BigDecimal("0.00022274000000000003"), WBNB),
        forContract = WalletAddress.unsafeFrom("0x163f182c32d24a09d91eb75820cde9fd5832b329"),
        hash = TransactionHash.unsafeApply("0xdc4b8148e8d106014e9f37be3b06746e04f432a271067c7f5a4eaa6ead729899"),
        timestamp = Instant.parse("2021-05-25T02:09:01Z")
      )
      assert(approval)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as Buy") {
      val transaction = getTransaction("/covalent/transactions/buy.json")
      val buyAndTransferIn = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("0.3875408729003106190"), WBNB),
          received = FungibleData(BigDecimal("207074895.8730373970"), Currency.unsafeFrom("FOOFIGHT")),
          coinAddress = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
          fee = FungibleData(BigDecimal("0.00377384"), WBNB),
          hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
          timestamp = Instant.parse("2021-10-18T12:27:24Z")
        ),
        TransferIn(
          value = FungibleData(BigDecimal("3.4736516581719474730"), Currency.unsafeFrom("BUSD")),
          receivedFrom = WalletAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56"),
          fee = FungibleData.zero(WBNB),
          hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
          timestamp = Instant.parse("2021-10-18T12:27:24Z")
        )
      )
      assert(buyAndTransferIn)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as Buy with multiple transfer-ins") {
      val transaction = getTransaction("/covalent/transactions/buy_with_multiple_transfers.json")
      val buyAndTransferIns = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("1.3000000000000000000"), WBNB),
          received = FungibleData(BigDecimal("10093678.5108933023485965940"), Currency.unsafeFrom("EMPDOGE")),
          coinAddress = WalletAddress.unsafeFrom("0x0d1cd07e959a701dfd361c98d40ce48691d8718c"),
          fee = FungibleData(BigDecimal("0.003242655"), WBNB),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        ),
        TransferIn(
          value =  FungibleData(BigDecimal("3.561490390"), Currency.unsafeFrom("DOGE")),
          receivedFrom = WalletAddress.unsafeFrom("0xba2ae424d960c26247dd6c32edc70b295c744c43"),
          fee = FungibleData.zero(WBNB),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        ),
        TransferIn(
          value =  FungibleData(BigDecimal("10093678.5108933023485965940"), Currency.unsafeFrom("EMPDOGE_Dividend_Tracker")),
          receivedFrom = WalletAddress.unsafeFrom("0x668cff8bbf5a18be1d561a25c7b10de213372698"),
          fee = FungibleData.zero(WBNB),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        )
      )
      assert(buyAndTransferIns)(isRight(hasSameElements(expected)))
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
      assert(claim)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as Contribute") {
      val transaction = getTransaction("/covalent/transactions/contribute.json")
      val contribute  = PositionEntry.fromTransaction(transaction, Address)
      val expected = Contribute(
        spent = FungibleData(BigDecimal("0.1023923391846748000"), WBNB),
        to = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
        fee = FungibleData(BigDecimal("0.0010398500000000001"), WBNB),
        hash = TransactionHash.unsafeApply("0x2bba1a27a5b3e4f96316506c41e85d882e71ae900ebd08f67cfca750de38460d"),
        timestamp = Instant.parse("2021-10-07T21:45:33Z")
      )
      assert(contribute)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as SELL of claimed tokens") {
      val transaction = getTransaction("/covalent/transactions/sell_after_claim.json")
      val sell = PositionEntry.fromTransaction(transaction, Address)
      val expected = Sell(
        sold = FungibleData(BigDecimal("9335310526620.73190"), Currency.unsafeFrom("NONO")),
        received = FungibleData(BigDecimal("925.5416553017160106510"), Currency.unsafeFrom("BUSD")),
        fee = FungibleData(BigDecimal("0.0036912200000000003"), WBNB),
        hash = TransactionHash.unsafeApply("0x5ec20963f3fe609c288d12319f45385a633243b9db4f20123ff8a933a93a2df3"),
        timestamp = Instant.parse("2021-10-08T19:13:37Z")
      )
      assert(sell)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as Sell") {
      val transaction = getTransaction("/covalent/transactions/sell1.json")
      val sell = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Sell(
          sold = FungibleData(BigDecimal("432156304.4306867440"), Currency.unsafeFrom("FOOFIGHT")),
          received = FungibleData(BigDecimal("0.4175743142140909320"), WBNB),
          fee = FungibleData(BigDecimal("0.00496212"), WBNB),
          hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
          timestamp = Instant.parse("2021-10-18T14:04:18Z")
        ),
        TransferIn(
          value = FungibleData(BigDecimal("1.7172675723509523810"), Currency.unsafeFrom("BUSD")),
          fee = FungibleData.zero(WBNB),
          receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
          hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
          timestamp = Instant.parse("2021-10-18T14:04:18Z")
        )
      )
      assert(sell)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as TransferIn") {
      val transaction = getTransaction("/covalent/transactions/transferIn.json")
      val transferIn  = PositionEntry.fromTransaction(transaction, Address)
      val expected = TransferIn(
        value = FungibleData(BigDecimal("0.001"), WBNB),
        fee = FungibleData(BigDecimal("0.000105"), WBNB),
        receivedFrom = WalletAddress.unsafeFrom("0x0c21496dcac5826c43747c39461e5dc9461e894a"),
        hash = TransactionHash.unsafeApply("0xb80792b60eeb04747eca64ea337ce6afcbf5e2bde350e89ed11a5f456e0fa2c8"),
        timestamp = Instant.parse("2021-05-21T10:21:02Z")
      )
      assert(transferIn)(isRight(hasSameElements(List(expected))))
    },
    test("Interpret transaction as TransferOut") {
      val transaction = getTransaction("/covalent/transactions/transferOut.json")
      val transferOut  = PositionEntry.fromTransaction(transaction, Address)
      val expected = TransferOut(
        amount = FungibleData(BigDecimal("3.0000000000000000000"), Currency.unsafeFrom("USDT")),
        fee = FungibleData(BigDecimal("0.0005780500000000001"), WBNB),
        to = WalletAddress.unsafeFrom("0x31efc4aeaa7c39e54a33fdc3c46ee2bd70ae0a09"),
        hash = TransactionHash.unsafeApply("0x01dc748a38922295ef5090a627c3b6125f06b166fada29b4d996a34af1fb05bc"),
        timestamp = Instant.parse("2021-09-05T17:51:04Z")
      )
      assert(transferOut)(isRight(hasSameElements(List(expected))))
    }
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
