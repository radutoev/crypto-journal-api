package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import domain.pricequote.PriceQuote

import DatastorePriceQuoteRepo.PriceQuoteBase
import zio.test.Assertion._
import zio.test.TestAspect.ignore
import zio.test._

import java.time.Instant

object DatastorePriceQuoteSpec extends DefaultRunnableSpec {
  override def spec = suite("DatastorePriceQuoteSpec") {
    suite("Entity creation") {
      test("Ancestor quotes") {
        val quotes   = List(
          PriceQuote(0.3d, Instant.parse("2021-04-05T15:23:00.000Z")),
          PriceQuote(2.3d, Instant.parse("2021-07-05T10:21:00.000Z")),
          PriceQuote(6.3d, Instant.parse("2021-07-05T12:21:00.000Z")),
          PriceQuote(7d, Instant.parse("2021-07-05T18:20:00.000Z")),
          PriceQuote(0.2d, Instant.parse("2021-07-05T18:30:00.000Z")),
        )
        val entities = DatastorePriceQuoteRepo.generateDatastoreQuotes(quotes)
        assert(entities)(hasSameElementsDistinct(Expected))
      } @@ ignore //Looks like recursive equality doesn't work.
    }
  }

  val Item1 = PriceQuoteBase(
    Instant.parse("2021-04-05T00:00:00.000Z"),
    0.3d,
    List(
      PriceQuoteBase(
        Instant.parse("2021-04-05T15:00:00.000Z"),
        0.3d,
        List(
          PriceQuoteBase(Instant.parse("2021-04-05T15:23:00.000Z"), 0.3d, Nil)
        )
      )
    )
  )

  val SubItem1 = PriceQuoteBase(
    Instant.parse("2021-07-05T10:00:00.000Z"),
    2.3d,
    List(
      PriceQuoteBase(
        Instant.parse("2021-07-05T10:21:00.000Z"),
        2.3d,
        Nil
      )
    )
  )

  val SubItem2 = PriceQuoteBase(
    Instant.parse("2021-07-05T12:00:00.000Z"),
    6.3d,
    List(
      PriceQuoteBase(
        Instant.parse("2021-07-05T12:21:00.000Z"),
        6.3d,
        Nil
      )
    )
  )

  val SubItem3 = PriceQuoteBase(
    Instant.parse("2021-07-05T18:00:00.000Z"),
    3.6d,
    List(
      PriceQuoteBase(
        Instant.parse("2021-07-05T18:20:00.000Z"),
        7d,
        Nil
      ),
      PriceQuoteBase(
        Instant.parse("2021-07-05T18:30:00.000Z"),
        0.2d,
        Nil
      )
    )
  )

  val Item2 = PriceQuoteBase(
    Instant.parse("2021-07-05T00:00:00.000Z"),
    3.95d,
    List(SubItem1, SubItem2, SubItem3)
  )

  private val Expected = List(Item1, Item2)
}
