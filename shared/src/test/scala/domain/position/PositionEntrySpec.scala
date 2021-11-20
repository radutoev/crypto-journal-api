package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.position.model.CoinName

import zio.test.Assertion._
import zio.test._

import java.time.Instant

object PositionEntrySpec extends DefaultRunnableSpec with FileOps {
  private val Address = WalletAddress.unsafeFrom("0x627909adab1ab107b59a22e7ddd15e5d9029bc41")
  override def spec = suite("PositionEntrySpec")(
    test("Interpret transaction as AirDrop") {
      val transaction = getTransaction("/covalent/transactions/airDrop.json")
      val airDrop     = PositionEntry.fromTransaction(transaction, Address)
      val expected = AirDrop(
        name = CoinName.unsafeApply("Elon Doge"),
        receivedFrom = WalletAddress.unsafeFrom("0xc6af635d908529d3c00a857079d9547aeecac400"),
        fee = FungibleData(BigDecimal(0.08568792), WBNB),
        received = FungibleData(BigDecimal("1000000000.0000000000000000000"), Currency.unsafeFrom("EDOGE")),
        coinAddress = CoinAddress.unsafeFrom("0xe7629f06171842b0b91fb46d4ed43d018ff1e38d"),
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
      val transaction      = getTransaction("/covalent/transactions/buy.json")
      val buyAndTransferIn = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("0.3875408729003106190"), WBNB),
          received = FungibleData(BigDecimal("207074895.8730373970"), Currency.unsafeFrom("FOOFIGHT")),
          receivedFrom = WalletAddress.unsafeFrom("0x845130e515f682fbb497c7d01c658dc344265c15"),
          name = CoinName.unsafeApply("Fruit Fighters"),
          coinAddress = CoinAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
          fee = FungibleData(BigDecimal("0.00377384"), WBNB),
          hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
          timestamp = Instant.parse("2021-10-18T12:27:24Z")
        ),
        TransferIn(
          value = FungibleData(BigDecimal("3.4736516581719474730"), Currency.unsafeFrom("BUSD")),
          name = Some(CoinName.unsafeApply("BUSD Token")),
          receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
          fee = FungibleData.zero(WBNB),
          coinAddress = Some(CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56")),
          hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
          timestamp = Instant.parse("2021-10-18T12:27:24Z")
        )
      )
      assert(buyAndTransferIn)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as Buy with multiple transfer-ins") {
      val transaction       = getTransaction("/covalent/transactions/buy_with_multiple_transfers.json")
      val buyAndTransferIns = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("1.3000000000000000000"), WBNB),
          received = FungibleData(BigDecimal("10093678.5108933023485965940"), Currency.unsafeFrom("EMPDOGE")),
          receivedFrom = WalletAddress.unsafeFrom("0xe9d53ff96b991f9025f90844b11b3eca5047bbb0"),
          fee = FungibleData(BigDecimal("0.003242655"), WBNB),
          name = CoinName.unsafeApply("Emperor Doge"),
          coinAddress = CoinAddress.unsafeFrom("0x0d1cd07e959a701dfd361c98d40ce48691d8718c"),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        ),
        TransferIn(
          value = FungibleData(BigDecimal("3.561490390"), Currency.unsafeFrom("DOGE")),
          receivedFrom = WalletAddress.unsafeFrom("0x668cff8bbf5a18be1d561a25c7b10de213372698"),
          fee = FungibleData.zero(WBNB),
          name = Some(CoinName.unsafeApply("Dogecoin")),
          coinAddress = Some(CoinAddress.unsafeFrom("0xba2ae424d960c26247dd6c32edc70b295c744c43")),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        ),
        TransferIn(
          value = FungibleData(BigDecimal("10093678.5108933023485965940"), Currency.unsafeFrom("EMPDOGE_Dividend_Tracker")),
          receivedFrom = WalletAddress.unsafeFrom("0x0000000000000000000000000000000000000000"),
          fee = FungibleData.zero(WBNB),
          name = Some(CoinName.unsafeApply("EMPDOGE_Dividend_Tracker")),
          coinAddress = Some(CoinAddress.unsafeFrom("0x668cff8bbf5a18be1d561a25c7b10de213372698")),
          hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
          timestamp = Instant.parse("2021-10-14T18:42:11Z")
        )
      )
      assert(buyAndTransferIns)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as Buy with other coin") {
      val transaction = getTransaction("/covalent/transactions/buy_with_busd.json")
      val buys        = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("1.575485922586737678"), WBNB),
          spentOriginal = Some(FungibleData(BigDecimal("650.0000000000000000000"), Currency.unsafeFrom("BUSD"))),
          received = FungibleData(BigDecimal("871.7809041660"), Currency.unsafeFrom("RPST")),
          receivedFrom = WalletAddress.unsafeFrom("0xe85132db5fed97dbb1fce7e397b4fa1b0b82cb64"),
          coinAddress = CoinAddress.unsafeFrom("0x4fb431848e8d36978c3ab89c5a2140f877fd155c"),
          name = CoinName.unsafeApply("Rock Paper Scissors Token"),
          fee = FungibleData(BigDecimal("0.004284840000000001"), WBNB),
          hash = TransactionHash.unsafeApply("0x1bf3ed05449b4c72d5c37e733490cb8ef58df09cab8f8f9aedef0d3511371dc3"),
          timestamp = Instant.parse("2021-10-11T19:46:45Z")
        )
      )
      assert(buys)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as Buy with multiple hops") {
      val transactions = getTransactions("/covalent/transactions/buy_with_multiple_hops.json")
      val buys = PositionEntry.fromTransaction(transactions(0), Address).
        map(partial => partial ++ PositionEntry.fromTransaction(transactions(1), Address).right.get)
      val expected = List(
        Buy(
          spent = FungibleData(BigDecimal("0.4610961971746591590"), WBNB),
          spentOriginal = Some(FungibleData(BigDecimal("216.5751328334734103600"), Currency.unsafeFrom("BUSD"))),
          received = FungibleData(BigDecimal("21499.0975321209498644260"), Currency.unsafeFrom("ZIN")),
          receivedFrom = WalletAddress.unsafeFrom("0x8a61c758023e6bc0841203751635c076baff03b3"),
          coinAddress = CoinAddress.unsafeFrom("0xfbe0b4ae6e5a200c36a341299604d5f71a5f0a48"),
          name = CoinName.unsafeApply("ZomaInfinity"),
          fee = FungibleData(BigDecimal("0.000938855"), WBNB),
          hash = TransactionHash.unsafeApply("0x3aa6ddfd7b66c1ab72a6f30dc1e1c65490049c1e99e55191c7f567ab52517096"),
          timestamp = Instant.parse("2021-08-31T19:05:16Z")
        ),
        //this is ignoring the Cake expenditure
        Buy(
          spent = FungibleData(BigDecimal("0.416813186602036685"), WBNB),
          spentOriginal = Some(FungibleData(BigDecimal("194.2794489739126615920"), Currency.unsafeFrom("BUSD"))),
          received = FungibleData(BigDecimal("23075.8463782345181451960"), Currency.unsafeFrom("ZIN")),
          receivedFrom = WalletAddress.unsafeFrom("0x8a61c758023e6bc0841203751635c076baff03b3"),
          coinAddress = CoinAddress.unsafeFrom("0xfbe0b4ae6e5a200c36a341299604d5f71a5f0a48"),
          name = CoinName.unsafeApply("ZomaInfinity"),
          fee = FungibleData(BigDecimal("0.0013576500000000002"), WBNB),
          hash = TransactionHash.unsafeApply("0xadafc5acc8243c0d153a7fa8c1f3ea788cb59ed10157b6bedbc16591a59ea6b5"),
          timestamp = Instant.parse("2021-08-31T17:37:46Z")
        ),
      )
      assert(buys)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as Claim") {
      val transaction = getTransaction("/covalent/transactions/claim.json")
      val claim       = PositionEntry.fromTransaction(transaction, Address)
      val expected = Claim(
        fee = FungibleData(BigDecimal("0.0028749400000000003"), WBNB),
        received = FungibleData(BigDecimal("9335310526620.732"), Currency.unsafeFrom("NONO")),
        receivedFrom = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
        name = CoinName.unsafeApply("CardaNONOmics"),
        coinAddress = CoinAddress.unsafeFrom("0x94afac344e579924d34d9429cbf5fec266f09a8b"),
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
      val sell        = PositionEntry.fromTransaction(transaction, Address)
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
      val sell        = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        TransferIn(
          value = FungibleData(BigDecimal("1.7172675723509523810"), Currency.unsafeFrom("BUSD")),
          fee = FungibleData.zero(WBNB),
          receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
          name = Some(CoinName.unsafeApply("BUSD Token")),
          coinAddress = Some(CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56")),
          hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
          timestamp = Instant.parse("2021-10-18T14:04:18Z")
        ),
        Sell(
          sold = FungibleData(BigDecimal("432156304.4306867440"), Currency.unsafeFrom("FOOFIGHT")),
          received = FungibleData(BigDecimal("0.4175743142140909320"), WBNB),
          fee = FungibleData(BigDecimal("0.00496212"), WBNB),
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
    test("Interpret refund as TransferIn") {
      val transaction = getTransaction("/covalent/transactions/refund_as_transferIn.json")
      val transferIns = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        TransferIn(
          value = FungibleData(BigDecimal("0.1970000000000000000"), WBNB),
          fee = FungibleData(BigDecimal("0.00015249"), WBNB),
          receivedFrom = WalletAddress.unsafeFrom("0x4a1e8df79fb01e1e127bf0a8640418cfe8916b7c"),
          hash = TransactionHash.unsafeApply("0xe1140a79a9ada19379b9755a0946582265d97b5f15e04469a3c03ce67065e76c"),
          timestamp = Instant.parse("2021-08-10T22:00:52Z")
        )
      )
      assert(transferIns)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as TransferIn when receiving tokens in a transaction not initiated by the address") {
      val transaction = getTransaction("/covalent/transactions/transferIn_from_another_tx.json")
      val transferIns = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        TransferIn(
          value = FungibleData(BigDecimal("5.413473528139433736"), Currency.unsafeFrom("BUSD")),
          fee = FungibleData.zero(WBNB),
          receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
          hash = TransactionHash.unsafeApply("0x18481a5b77c7008773ecadf47106925ecdb1a22d34269a3c059165be0e53c303"),
          timestamp = Instant.parse("2021-10-18T13:30:09Z")
        )
      )
      assert(transferIns)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as TransferOut") {
      val transaction  = getTransaction("/covalent/transactions/transferOut.json")
      val transferOuts = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        TransferOut(
          amount = FungibleData(BigDecimal("3.0000000000000000000"), Currency.unsafeFrom("USDT")),
          fee = FungibleData(BigDecimal("0.0005780500000000001"), WBNB),
          to = WalletAddress.unsafeFrom("0x31efc4aeaa7c39e54a33fdc3c46ee2bd70ae0a09"),
          hash = TransactionHash.unsafeApply("0x01dc748a38922295ef5090a627c3b6125f06b166fada29b4d996a34af1fb05bc"),
          timestamp = Instant.parse("2021-09-05T17:51:04Z")
        )
      )
      assert(transferOuts)(isRight(hasSameElements(expected)))
    },
    test("Interpret transaction as TransferOut from TokenPurchase") {
      val transaction     = getTransaction("/covalent/transactions/token_purchase_as_transferOut.json")
      val transferOutList = PositionEntry.fromTransaction(transaction, Address)
      val expected = List(
        TransferOut(
          amount = FungibleData(BigDecimal("0.5"), WBNB),
          to = WalletAddress.unsafeFrom("0x639acba969203137d173d89031a73c60721e5e11"),
          fee = FungibleData(BigDecimal("0.0006087"), WBNB),
          hash = TransactionHash.unsafeApply("0x33229054c2e5998cc35a6775e5659ddad997a26474eb8f1c615f3ce79b14ff1b"),
          timestamp = Instant.parse("2021-09-17T17:00:58Z")
        )
      )
      assert(transferOutList)(isRight(hasSameElements(expected)))
    }
  )
}
