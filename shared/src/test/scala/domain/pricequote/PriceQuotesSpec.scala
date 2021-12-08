package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{Currency, USDT, WBNB}

import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PriceQuotesSpec extends DefaultRunnableSpec {
  override def spec = suite("PriceQuotes")(
    suite("Find price")(
      test("No price if no quotes") {
        assert(PriceQuotes.empty().findPrice(CurrencyPair(WBNB, USDT), Instant.now()))(isNone)
      },
      test("No price if pair not found") {
        val timestamp = Instant.now()
        val lookup = CurrencyPair(Currency.unsafeFrom("XLM"), Currency.unsafeFrom("STR"))
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List(PriceQuote(2d, timestamp)))
        assert(priceQuotes.findPrice(lookup, timestamp))(isNone)
      },
      test("No price for missing quotes") {
        val timestamp = Instant.now()
        val lookup = CurrencyPair(WBNB, USDT)
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List.empty)
        assert(priceQuotes.findPrice(lookup, timestamp))(isNone)
      },
      test("Price for matching currency pair") {
        val timestamp = Instant.now()
        val lookup = CurrencyPair(WBNB, USDT)
        val priceQuotes = PriceQuotes(CurrencyPair(WBNB, USDT), List(PriceQuote(3d, timestamp)))
        assert(priceQuotes.findPrice(lookup, timestamp))(isSome(equalTo(PriceQuote(3d, timestamp))))
      }
    )
  )
}
