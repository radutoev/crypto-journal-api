package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{USDT, WBNB}

import zio.test._
import zio.test.Assertion._

import java.time.Instant

object PriceQuotesSpec extends DefaultRunnableSpec {
  override def spec = suite("PriceQuotes")(
    suite("Find price")(
      test("No data if no price quotes") {
        assert(PriceQuotes.empty().findPrice(CurrencyPair(WBNB, USDT), Instant.now()))(isNone)
      }
    )
  )
}
