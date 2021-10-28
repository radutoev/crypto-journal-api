package io.softwarechain.cryptojournal
package domain.position

import Generators._
import domain.model._

import eu.timepit.refined.refineV
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
      check(Gen.listOfN(3)(genPosition), genOpenPositionEntry, genClosedPositionEntry) {
        (positions, e1, e2) =>
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
      check(Gen.listOfN(3)(genPosition), genOpenPositionEntry, genOpenPositionEntry) {
        (positions, e1, e2) =>
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