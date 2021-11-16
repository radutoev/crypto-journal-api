package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.position.MarketPlays.findMarketPlays
import domain.position.model.CoinName
import infrastructure.covalent.dto._

import eu.timepit.refined.refineV
import zio.json._
import zio.test.Assertion._
import zio.test._

import java.time.Instant

object MarketPlayGeneratorSpec extends DefaultRunnableSpec with FileOps {
  private val Address = refineV[WalletAddressPredicate].unsafeFrom("0x627909adab1ab107b59a22e7ddd15e5d9029bc41")

  override def spec = suite("MarketPlaysGeneratorSpec")(
    test("Generate position from contribution and claim") {
      val transactions = getTransactions("/covalent/position_generation/contribute_claim_sell.json")
      val marketPlays  = findMarketPlays(Address, transactions)
      val expected = List(
        Position.unsafeApply(
          entries = List(
            Contribute(
              spent = FungibleData(BigDecimal("0.1023923391846748000"), WBNB),
              to = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
              fee = FungibleData(BigDecimal("0.0010398500000000001"), WBNB),
              hash = TransactionHash.unsafeApply("0x2bba1a27a5b3e4f96316506c41e85d882e71ae900ebd08f67cfca750de38460d"),
              timestamp = Instant.parse("2021-10-07T21:45:33Z")
            ),
            Contribute(
              spent = FungibleData(BigDecimal("0.1023923391846748000"), WBNB),
              to = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
              fee = FungibleData(BigDecimal("0.00032141"), WBNB),
              hash = TransactionHash.unsafeApply("0x56ff303b40e520df3fc62d12587abc09ceec6aff579b7a2d6074ed48e4f79da0"),
              timestamp = Instant.parse("2021-10-07T21:46:54Z")
            ),
            Claim(
              fee = FungibleData(BigDecimal("0.0028749400000000003"), WBNB),
              received = FungibleData(BigDecimal("9335310526620.73200"), Currency.unsafeFrom("NONO")),
              receivedFrom = WalletAddress.unsafeFrom("0x42f8c2113f833db5886eea67a77ab32d83f4339f"),
              hash = TransactionHash.unsafeApply("0x1f88b75a26cab0bba6d1c8468559ad392af82c37e45b27ce07ca98d36b59d0c5"),
              timestamp = Instant.parse("2021-10-08T09:46:25Z"),
              name = CoinName.unsafeApply("CardaNONOmics"),
              coinAddress = CoinAddress.unsafeFrom("0x94afac344e579924d34d9429cbf5fec266f09a8b")
            ),
            Sell(
              sold = FungibleData(BigDecimal("9335310526620.73190"), Currency.unsafeFrom("NONO")),
              received = FungibleData(BigDecimal("925.5416553017160106510"), Currency.unsafeFrom("BUSD")),
              fee = FungibleData(BigDecimal("0.0036912200000000003"), WBNB),
              hash = TransactionHash.unsafeApply("0x5ec20963f3fe609c288d12319f45385a633243b9db4f20123ff8a933a93a2df3"),
              timestamp = Instant.parse("2021-10-08T19:13:37Z")
            )
          )
        )
      )
      assert(1)(equalTo(marketPlays.plays.size)) &&
      assert(expected)(hasSameElements(marketPlays.plays))
    },
    test("Generate TopUp") {
      val file         = readFile("/covalent/position_generation/transferIn.json").fromJson[List[Transaction]]
      val transactions = file.right.get
      val topUps       = findMarketPlays(Address, transactions.map(_.toDomain()))
      val expected = List(
        TopUp(
          txHash = TransactionHash.unsafeApply("0x5d7ec936bcefdaa9fbceef0fc7d6373b13756d6bc1ac91d72062cfe5c28d7e09"),
          value = FungibleData(BigDecimal("2.2700000000000000000"), WBNB),
          fee = FungibleData(BigDecimal(0.000105), WBNB),
          timestamp = Instant.parse("2021-05-21T10:57:17Z")
        )
      )
      assert(expected)(hasSameElements(topUps.plays))
    },
    test("Buys into open and closed positions") {
      val file         = readFile("/covalent/position_generation/buy_into_open_close.json").fromJson[List[Transaction]]
      val transactions = file.right.get
      val marketPlays  = findMarketPlays(Address, transactions.map(_.toDomain()))
      val expected = List(
        Position.unsafeApply(
          entries = List(
            Buy(
              spent = FungibleData(BigDecimal("1.3000000000000000000"), WBNB),
              received = FungibleData(BigDecimal("10093678.5108933023485965940"), Currency.unsafeFrom("EMPDOGE")),
              receivedFrom = WalletAddress.unsafeFrom("0xe9d53ff96b991f9025f90844b11b3eca5047bbb0"),
              coinAddress = CoinAddress.unsafeFrom("0x0d1cd07e959a701dfd361c98d40ce48691d8718c"),
              fee = FungibleData(BigDecimal("0.003242655"), WBNB),
              hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
              timestamp = Instant.parse("2021-10-14T18:42:11Z"),
              name = CoinName.unsafeApply("Emperor Doge"),
            )
          )
        ),
        Position.unsafeApply(
          entries = List(
            TransferIn(
              value = FungibleData(BigDecimal("3.561490390"), Currency.unsafeFrom("DOGE")),
              receivedFrom = WalletAddress.unsafeFrom("0x668cff8bbf5a18be1d561a25c7b10de213372698"),
              fee = FungibleData.zero(WBNB),
              hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
              timestamp = Instant.parse("2021-10-14T18:42:11Z"),
              name = Some(CoinName.unsafeApply("Dogecoin")),
              coinAddress = Some(CoinAddress.unsafeFrom("0xba2ae424d960c26247dd6c32edc70b295c744c43"))
            )
          )
        ),
        Position.unsafeApply(
          entries = List(
            TransferIn(
              value = FungibleData(
                BigDecimal("10093678.5108933023485965940"),
                Currency.unsafeFrom("EMPDOGE_Dividend_Tracker")
              ),
              receivedFrom = WalletAddress.unsafeFrom("0x0000000000000000000000000000000000000000"),
              fee = FungibleData.zero(WBNB),
              hash = TransactionHash.unsafeApply("0x27aaf173d99d0936faab0b71b28fb69ded43ca40e39dcc238591a40725c717b3"),
              timestamp = Instant.parse("2021-10-14T18:42:11Z"),
              name = Some(CoinName.unsafeApply("EMPDOGE_Dividend_Tracker")),
              coinAddress = Some(CoinAddress.unsafeFrom("0x668cff8bbf5a18be1d561a25c7b10de213372698"))
            )
          )
        ),
        Position.unsafeApply(
          entries = List(
            Buy(
              spent = FungibleData(BigDecimal("0.25"), WBNB),
              received = FungibleData(BigDecimal("225081408.557649347"), Currency.unsafeFrom("FOOFIGHT")),
              receivedFrom = WalletAddress.unsafeFrom("0x845130e515f682fbb497c7d01c658dc344265c15"),
              coinAddress = CoinAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
              fee = FungibleData(BigDecimal("0.00397604"), WBNB),
              hash = TransactionHash.unsafeApply("0x6ee328f30cc70dee6beaab4466f0ba3d9fbacbe14f5408bd2efdf1ef18d70c25"),
              timestamp = Instant.parse("2021-10-18T11:38:19Z"),
              name = CoinName.unsafeApply("Fruit Fighters")
            ),
            Approval(
              fee = FungibleData(BigDecimal("0.000222145"), WBNB),
              forContract = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
              hash = TransactionHash.unsafeApply("0xdd69a8eafff819014295a007af3ca923c14afc6a673ed42923243a7cc04579d9"),
              timestamp = Instant.parse("2021-10-18T11:38:55Z")
            ),
            Buy(
              spent = FungibleData(BigDecimal("0.3875408729003106190"), WBNB),
              received = FungibleData(BigDecimal("207074895.8730373970"), Currency.unsafeFrom("FOOFIGHT")),
              receivedFrom = WalletAddress.unsafeFrom("0x845130e515f682fbb497c7d01c658dc344265c15"),
              coinAddress = CoinAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
              fee = FungibleData(BigDecimal("0.00377384"), WBNB),
              hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
              timestamp = Instant.parse("2021-10-18T12:27:24Z"),
              name = CoinName.unsafeApply("Fruit Fighters")
            ),
            Sell(
              sold = FungibleData(BigDecimal("432156304.4306867440"), Currency.unsafeFrom("FOOFIGHT")),
              received = FungibleData(BigDecimal("0.4175743142140909320"), WBNB),
              fee = FungibleData(BigDecimal("0.00496212"), WBNB),
              hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
              timestamp = Instant.parse("2021-10-18T14:04:18Z")
            )
          )
        ),
        Position.unsafeApply(
          entries = List(
            TransferIn(
              value = FungibleData(BigDecimal("3.4736516581719474730"), Currency.unsafeFrom("BUSD")),
              receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
              fee = FungibleData.zero(WBNB),
              hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
              timestamp = Instant.parse("2021-10-18T12:27:24Z"),
              name = Some(CoinName.unsafeApply("BUSD Token")),
              coinAddress = Some(CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56"))
            ),
            TransferIn(
              value = FungibleData(BigDecimal("1.7172675723509523810"), Currency.unsafeFrom("BUSD")),
              fee = FungibleData.zero(WBNB),
              receivedFrom = WalletAddress.unsafeFrom("0x4d9ac32a8a701e11bf21d2b65de783ae74e0159a"),
              hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
              timestamp = Instant.parse("2021-10-18T14:04:18Z"),
              name = Some(CoinName.unsafeApply("BUSD Token")),
              coinAddress = Some(CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56"))
            )
          )
        )
      ).sortBy(_.openedAt)(Ordering[Instant])
      assert(expected.size)(equalTo(marketPlays.plays.size)) &&
      assert(expected)(hasSameElementsDistinct(marketPlays.plays))
    }
  )
}
