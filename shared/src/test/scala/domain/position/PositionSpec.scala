package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.{PriceQuote, PriceQuotes}
import vo.TimeInterval

import zio.test.Assertion._
import zio.test._

import java.time.Instant

object PositionSpec extends DefaultRunnableSpec {
  override def spec = suite("TopUpSpec")(
    test("Properties of a Position") {
      val position = PositionUnderTest
      assert(position.currency)(isSome(equalTo(Currency.unsafeFrom("FOOFIGHT")))) &&
        assert(position.numberOfExecutions)(equalTo(4)) &&
        assert(position.timeInterval)(equalTo(TimeInterval(Instant.parse("2021-10-18T11:38:19Z"), Instant.parse("2021-10-18T14:04:18Z")))) &&
        assert(position.cost)(equalTo(Map(WBNB -> FungibleData(BigDecimal("0.63754087290031064"), WBNB), USD -> FungibleData(BigDecimal("300.432966715797795248663236085984"), USD)))) &&
        assert(position.fees)(equalTo(Map(WBNB -> FungibleData(BigDecimal("0.012934145"), WBNB), USD -> FungibleData(BigDecimal("6.1753836674609374056540"), USD)))) &&
        assert(position.totalCost)(equalTo(Map(WBNB -> FungibleData(BigDecimal("0.65047501790031064"), WBNB), USD -> FungibleData(BigDecimal("306.608350383258732654317236085984"), USD)))) &&
        assert(position.fiatReturn)(isSome(equalTo(FungibleData(BigDecimal("-103.333179321181985343044042726609"), USD)))) &&
//        assert(position.fiatReturnPercentage)(isSome(equalTo())) &&
//        assert(position.totalCoins)(equalTo()) &&
//        assert(position.orderSize)(equalTo()) &&
//        assert(position.numberOfCoins)(equalTo()) &&
        assert(position.isWin)(isSome(equalTo(false))) &&
        assert(position.isLoss)(isSome(equalTo(true))) &&
        assert(position.state)(equalTo(Closed)) &&
        assert(position.isOpen)(equalTo(false)) &&
        assert(position.isClosed)(equalTo(true)) &&
        assert(position.openedAt)(equalTo(Instant.parse("2021-10-18T11:38:19Z"))) &&
        assert(position.closedAt)(isSome(equalTo(Instant.parse("2021-10-18T14:04:18Z"))))
//        assert(position.holdTime)(isSome(equalTo()))
    }
  )

  val PositionUnderTest = new Position(
    entries = List(
      Buy(
        fee = FungibleData(BigDecimal(0.00397604), WBNB),
        spent = FungibleData(BigDecimal(0.2500000000000000000), WBNB),
        received = FungibleData(BigDecimal(225081408.5576493470), Currency.unsafeFrom("FOOFIGHT")),
        coinAddress = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
        hash = TransactionHash.unsafeApply("0x6ee328f30cc70dee6beaab4466f0ba3d9fbacbe14f5408bd2efdf1ef18d70c25"),
        timestamp = Instant.parse("2021-10-18T11:38:19Z")
      ),
      Approval(
        fee = FungibleData(0.000222145, WBNB),
        forContract = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
        hash = TransactionHash.unsafeApply("0xdd69a8eafff819014295a007af3ca923c14afc6a673ed42923243a7cc04579d9"),
        timestamp = Instant.parse("2021-10-18T11:38:55Z"),
      ),
      Buy(
        fee = FungibleData(BigDecimal(0.00377384), WBNB),
        spent = FungibleData(BigDecimal(0.3875408729003106190), WBNB),
        received = FungibleData(BigDecimal(207074895.8730373970), Currency.unsafeFrom("FOOFIGHT")),
        coinAddress = WalletAddress.unsafeFrom("0x8c473a401e7ebde6dab178ea0bb5b35cde542c0e"),
        hash = TransactionHash.unsafeApply("0x6ab1f8414ccd57df5230e05dbda9e739f8d5369d26c77b8f6861949ef87dd212"),
        timestamp = Instant.parse("2021-10-18T12:27:24Z")
      ),
      Sell(
        sold = FungibleData(BigDecimal(432156304.430686744), Currency.unsafeFrom("FOOFIGHT")),
        received = FungibleData(BigDecimal(0.4175743142140909320), WBNB),
        fee = FungibleData(BigDecimal(0.00496212), WBNB),
        hash = TransactionHash.unsafeApply("0xe4adede1d150868f53aee2bf0973477f39a8531cdc34800ea4ad4fe6aacf8414"),
        timestamp = Instant.parse("2021-10-18T14:04:18Z")
      )
    ),
    priceQuotes = Some(PriceQuotes(List(
      PriceQuote(price = 473.00000000f, timestamp = Instant.parse("2021-10-18T11:00:00Z")),
      PriceQuote(price = 470.10000000f, timestamp = Instant.parse("2021-10-18T12:00:00Z")),
      PriceQuote(price = 486.80000000f, timestamp = Instant.parse("2021-10-18T14:00:00Z"))
    )))
  )
}
