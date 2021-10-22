package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.position.Generators._

import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import zio.random.Random
import zio.test.Assertion._
import zio.test._

import java.time.Instant

object MarketPlaysMergeSpec extends DefaultRunnableSpec {
  override def spec = suite("PositionsMergeSpec")(
    testM("Union the two sets if no common currency") {
      check(Gen.listOf(genPosition)) {
        positions =>
          val uniqueCurrencyPositions = positions.distinctBy(_.currency).sortBy(_.openedAt)(Ordering[Instant])

          if (uniqueCurrencyPositions.length > 1) {
            val random          = scala.util.Random.between(0, uniqueCurrencyPositions.length - 1)
            val (first, second) = uniqueCurrencyPositions.splitAt(random)
            val firstPositions  = MarketPlays(first)
            val secondPositions = MarketPlays(second)
            val result          = firstPositions.merge(secondPositions)
            assert(result.plays)(hasSameElements(positions))
          } else {
            assert(MarketPlays(uniqueCurrencyPositions).merge(MarketPlays.empty()).plays)(
              hasSameElements(uniqueCurrencyPositions)
            )
          }
      }
    },
    testM("Union the two sets if no open positions in other") {
      check(Gen.listOf(genPosition), genClosedPositionEntry) {
        (positions, closedEntry) =>
          if (positions.length > 1) {
            val (first, second) = positions.splitAt(scala.util.Random.between(0, positions.length - 1))
            val result = MarketPlays(first).merge(
              MarketPlays(second.map(pos => pos.copy(entries = List(closedEntry))))
            )
            assert(result.plays.length)(equalTo(positions.length))
          } else {
            assert(MarketPlays(positions).merge(MarketPlays.empty()).plays)(hasSameElements(positions))
          }
      }
    },
    testM("Merge and close open positions for matching coins") {
      check(Gen.listOfN(3)(genPosition), genOpenPositionEntry, genClosedPositionEntry) { (positions, e1, e2) =>
        val c1 = refineV[CurrencyPredicate].unsafeFrom("WBNB")
        val c2 = refineV[CurrencyPredicate].unsafeFrom("WBNB2")

        val p1 =
          MarketPlays(List(positions.head.copy(currency = c1, entries = List(e1)), positions(1).copy(currency = c2)))
        val p2 = MarketPlays(List(positions.last.copy(currency = c1, entries = List(e2))))

        val result = p2.merge(p1)

        assert(result.plays.length)(equalTo(2))
      }
    },
    testM("Merge open positions for matching coins") {
      check(Gen.listOfN(3)(genPosition), genOpenPositionEntry, genOpenPositionEntry) { (positions, e1, e2) =>
        val c1 = refineV[CurrencyPredicate].unsafeFrom("WBNB")
        val c2 = refineV[CurrencyPredicate].unsafeFrom("WBNB2")

        val p1 =
          MarketPlays(List(positions.head.copy(currency = c1, entries = List(e1)), positions(1).copy(currency = c2)))
        val p2 = MarketPlays(List(positions.last.copy(currency = c1, entries = List(e2))))

        val result = p2.merge(p1)

        assert(result.plays.length)(equalTo(2))
      }
    },
    testM("Merge closed positions for matching coins") {
      check(Gen.listOfN(3)(genPosition), genOpenPositionEntry, genClosedPositionEntry, genClosedPositionEntry) {
        (positions, e1, e2, e3) =>
          val c1 = refineV[CurrencyPredicate].unsafeFrom("WBNB")
          val c2 = refineV[CurrencyPredicate].unsafeFrom("WBNB2")

          val p1 = MarketPlays(
            List(positions.head.copy(currency = c1, entries = List(e1, e2)), positions(1).copy(currency = c2))
          )
          val p2 = MarketPlays(List(positions.last.copy(currency = c1, entries = List(e3))))

          val result = p2.merge(p1)

          assert(result.plays.length)(equalTo(2))
      }
    }
  )
}

object Generators {
  val genPositionId: Gen[Random, NonEmptyString] =
    Gen.stringBounded(2, 100)(Gen.alphaNumericChar).map(NonEmptyString.unsafeFrom)
  val genCurrency: Gen[Random with Sized, Currency] = Gen.stringN(10)(Gen.anyChar).map(NonEmptyString.unsafeFrom)
  val genEntryType: Gen[Random, TransactionType]    = Gen.boolean.map(x => if (x) Buy else Sell)
  val genTxHash                                     = Gen.stringBounded(2, 100)(Gen.alphaNumericChar).map(refineV[TransactionHashPredicate].unsafeFrom(_))

  val genFungibleData = for {
    amount   <- Gen.anyDouble.map(BigDecimal(_))
    currency <- genCurrency
  } yield FungibleData(amount, currency)

  val genPositionEntry = for {
    txType    <- genEntryType
    timestamp <- Gen.anyInstant
    value     <- genFungibleData
    fee       <- genFungibleData
    txHash    <- genTxHash
  } yield PositionEntry(txType, value, fee, timestamp, txHash, None)

  val genClosedPositionEntry = genPositionEntry.map(entry => entry.copy(`type` = Sell))

  val genOpenPositionEntry = genPositionEntry.map(entry => entry.copy(`type` = Buy))

  val genPosition = for {
    positionId <- genPositionId
    currency   <- genCurrency
    openedAt   <- Gen.anyInstant
    entries    <- Gen.listOfN(2)(genPositionEntry)
  } yield Position(currency, openedAt, entries, id = Some(positionId))
}
