package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{ Currency, FungibleData, BUSD, WBNB }

import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PriceQuotesSpec extends DefaultRunnableSpec {
  override def spec = suite("PriceQuotes")(
    suite("Quote value")(
      test("Matching currency pair") {
        val timestamp = Instant.now()
        val priceQuotes = PriceQuotes(
          CurrencyPair(WBNB, BUSD),
          List(PriceQuote(2d, timestamp.minusSeconds(2)), PriceQuote(3d, timestamp))
        )
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(3), WBNB), BUSD, timestamp))(
          isSome(equalTo(FungibleData(BigDecimal(9), BUSD)))
        )
      },
      test("Multi pair lookup") {
        val timestamp = Instant.now()
        val xlm       = Currency.unsafeFrom("XLM")
        val priceQuotes = PriceQuotes(
          Map(
            CurrencyPair(xlm, WBNB)  -> List(PriceQuote(5d, timestamp)),
            CurrencyPair(WBNB, BUSD) -> List(PriceQuote(2d, timestamp))
          )
        )
        //xlm -> bnb -> usdt = (0.09 * 5) * 2
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(0.09), xlm), BUSD, timestamp))(
          isSome(equalTo(FungibleData(BigDecimal(0.9), BUSD)))
        )
      },
      test("No value if no quotes") {
        assert(PriceQuotes.empty().quotedValue(FungibleData(BigDecimal(3), WBNB), BUSD, Instant.now()))(isNone)
      },
      test("No value if pair not found") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, BUSD), List(PriceQuote(2d, timestamp)))
        assert(
          priceQuotes
            .quotedValue(FungibleData(BigDecimal(2), Currency.unsafeFrom("XLM")), Currency.unsafeFrom("STR"), timestamp)
        )(isNone)
      },
      test("No value for missing quotes") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, BUSD), List.empty)
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(2), WBNB), BUSD, timestamp))(isNone)
      }
    ),
    suite("Find price")(
      test("Find direct pair") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, BUSD), List(PriceQuote(3.2d, timestamp)))
        assert(priceQuotes.findPrice(CurrencyPair(WBNB, BUSD), timestamp))(isSome(equalTo(PriceQuote(3.2d, timestamp))))
      },
      test("Find for multi-lookup pair") {
        val timestamp   = Instant.now()
        val xlm       = Currency.unsafeFrom("XLM")
        val priceQuotes = PriceQuotes(
          Map(
            CurrencyPair(xlm, WBNB)  -> List(PriceQuote(0.5d, timestamp)),
            CurrencyPair(WBNB, BUSD) -> List(PriceQuote(0.2d, timestamp))
          )
        )
        assert(priceQuotes.findPrice(CurrencyPair(xlm, BUSD), timestamp))(isSome(equalTo(PriceQuote(0.1d, timestamp))))
      }
    ),
    suite("Merge PriceQuotes")(
      test("Merge with empty") {
        val timestamp   = Instant.now()
        val pair = CurrencyPair(WBNB, BUSD)
        val result = PriceQuotes(Map(pair -> List(PriceQuote(0.3d, timestamp)))).merge(PriceQuotes.empty())
        val expected = PriceQuotes(Map(pair -> List(PriceQuote(0.3d, timestamp))))
        assert(result)(equalTo(expected))
      },
      test("Merge new currencies") {
        val timestamp   = Instant.now()
        val pair1 = CurrencyPair(WBNB, BUSD)
        val pair2 = CurrencyPair(WBNB, Currency.unsafeFrom("ACoin"))
        val result = PriceQuotes(Map(pair1 -> List(PriceQuote(0.3d, timestamp))))
          .merge(PriceQuotes(Map(pair2 -> List(PriceQuote(0.7d, timestamp)))))
        val expected = PriceQuotes(Map(
          pair1 -> List(PriceQuote(0.3d, timestamp)),
          pair2 -> List(PriceQuote(0.7d, timestamp))
        ))
        assert(result)(equalTo(expected))
      },
      test("Merge existent currencies, original has priority") {
        val timestamp   = Instant.now()
        val pair = CurrencyPair(WBNB, BUSD)
        val result = PriceQuotes(Map(pair -> List(PriceQuote(0.3d, timestamp))))
          .merge(PriceQuotes(Map(pair -> List(PriceQuote(0.2d, timestamp.minusSeconds(10)), PriceQuote(0.9d, timestamp)))))
        val expected = PriceQuotes(
          Map(pair -> List(PriceQuote(0.2d, timestamp.minusSeconds(10)), PriceQuote(0.3d, timestamp)))
        )
        assert(result)(equalTo(expected))
      }
    )
  )
}
