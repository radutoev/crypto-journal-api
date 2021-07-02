package io.softwarechain.cryptojournal
package domain.position

import domain.model._

import Generators._
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import zio.random.Random
import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PositionsMergeSpec extends DefaultRunnableSpec {
  override def spec = suite("PositionsMergeSpec")(
    testM("Union the two sets if no common currency") {
      check(Gen.listOf(genPosition)) {
        positions =>
          val uniqueCurrencyPositions = positions.distinctBy(_.currency).sortBy(_.openedAt)(Ordering[Instant])

          if (uniqueCurrencyPositions.length > 1) {
            val random          = scala.util.Random.between(0, uniqueCurrencyPositions.length - 1)
            val (first, second) = uniqueCurrencyPositions.splitAt(random)
            val firstPositions  = Positions(first)
            val secondPositions = Positions(second)
            val result          = firstPositions.merge(secondPositions)
            assert(result.items)(hasSameElements(positions))
          } else {
            assert(Positions(uniqueCurrencyPositions).merge(Positions.empty()).items)(
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
            val result = Positions(first).merge(
              Positions(second.map(pos => pos.copy(entries = List(closedEntry))))
            )
            assert(result.items.length)(equalTo(positions.length))
          } else {
            assert(Positions(positions).merge(Positions.empty()).items)(hasSameElements(positions))
          }
      }
    }

//    testM("Close open positions for matching coins") {
//      check(genPosition, genPosition, genPosition) { (pos1, pos2, pos3) =>
//        val c1 = refineV[CurrencyPredicate].unsafeFrom("WBNB")
//
//        val p1 = Positions(List(pos1.copy(currency = c1,), pos2))
//        val p2 = Positions(List(pos3))
//      }
//    }
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

  val genPosition = for {
    positionId <- genPositionId
    currency   <- genCurrency
    openedAt   <- Gen.anyInstant
    closedAt   <- Gen.option(Gen.anyShort).map(_.map(x => openedAt.plusSeconds(x)))
    entries    <- Gen.listOf(genPositionEntry)
  } yield Position(currency, openedAt, closedAt, entries, None, Some(positionId))
}
