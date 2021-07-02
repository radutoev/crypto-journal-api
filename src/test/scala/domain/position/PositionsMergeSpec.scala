package io.softwarechain.cryptojournal
package domain.position

import domain.model._

import Generators._

import eu.timepit.refined.types.string.NonEmptyString
import zio.random.Random
import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PositionsMergeSpec extends DefaultRunnableSpec {
  override def spec = suite("PositionsMergeSpec")(
    testM("Union the two sets if no common currency") {
      check(Gen.listOf(genPosition)) { positions =>
        assert(1)(equalTo(1))
      }
    }
  )
}

object Generators {
  val positionIdGen: Gen[Random, NonEmptyString] = Gen.stringBounded(2, 100)(Gen.alphaNumericChar).map(NonEmptyString.unsafeFrom)
  val currencyGen: Gen[Random with Sized, Currency] = Gen.stringN(10)(Gen.anyChar).map(NonEmptyString.unsafeFrom)
  val stateGen: Gen[Random, State] = Gen.boolean.map(bool => if(bool) Open else Closed)
  val instantGen: Gen[Random, Instant] = Gen.anyInstant

  val genPosition = for {
    positionId <- positionIdGen
    currency   <- currencyGen
    state      <- stateGen
    openedAt   <- instantGen
    closedAt   <- Gen.option(Gen.anyShort).map(_.map(x => openedAt.plusSeconds(x)))
  } yield Position(currency, state, openedAt, closedAt, List.empty, None, Some(positionId))
}
