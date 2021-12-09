package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{ Currency, FungibleData, USDT, WBNB }

import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PriceQuotesSpec extends DefaultRunnableSpec {
  override def spec = suite("PriceQuotes")(
    suite("Quote value")(
      test("Matching currency pair") {
        val timestamp = Instant.now()
        val priceQuotes = PriceQuotes(
          CurrencyPair(WBNB, USDT),
          List(PriceQuote(2d, timestamp.minusSeconds(2)), PriceQuote(3d, timestamp))
        )
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(3), WBNB), USDT, timestamp))(
          isSome(equalTo(FungibleData(BigDecimal(9), USDT)))
        )
      },
      test("Multi pair lookup") {
        val timestamp = Instant.now()
        val xlm       = Currency.unsafeFrom("XLM")
        val priceQuotes = PriceQuotes(
          Map(
            CurrencyPair(xlm, WBNB)  -> List(PriceQuote(5d, timestamp)),
            CurrencyPair(WBNB, USDT) -> List(PriceQuote(2d, timestamp))
          )
        )
        //xlm -> bnb -> usdt = (0.09 * 5) * 2
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(0.09), xlm), USDT, timestamp))(
          isSome(equalTo(FungibleData(BigDecimal(0.9), USDT)))
        )
      },
      test("No value if no quotes") {
        assert(PriceQuotes.empty().quotedValue(FungibleData(BigDecimal(3), WBNB), USDT, Instant.now()))(isNone)
      },
      test("No value if pair not found") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List(PriceQuote(2d, timestamp)))
        assert(
          priceQuotes
            .quotedValue(FungibleData(BigDecimal(2), Currency.unsafeFrom("XLM")), Currency.unsafeFrom("STR"), timestamp)
        )(isNone)
      },
      test("No value for missing quotes") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List.empty)
        assert(priceQuotes.quotedValue(FungibleData(BigDecimal(2), WBNB), USDT, timestamp))(isNone)
      }
    ),
    suite("Find price")(
      test("Find direct pair") {
        val timestamp   = Instant.now()
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List(PriceQuote(3.2d, timestamp)))
        assert(priceQuotes.findPrice(CurrencyPair(WBNB, USDT), timestamp))(isSome(equalTo(PriceQuote(3.2d, timestamp))))
      },
      test("Find for multi-lookup pair") {
        val timestamp   = Instant.now()
        val xlm       = Currency.unsafeFrom("XLM")
        val priceQuotes = PriceQuotes(
          Map(
            CurrencyPair(xlm, WBNB)  -> List(PriceQuote(0.5d, timestamp)),
            CurrencyPair(WBNB, USDT) -> List(PriceQuote(0.2d, timestamp))
          )
        )
        assert(priceQuotes.findPrice(CurrencyPair(xlm, USDT), timestamp))(isSome(equalTo(PriceQuote(0.1d, timestamp))))
      }
    )
  )
}
