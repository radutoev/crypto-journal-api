package io.softwarechain.cryptojournal
package domain.position

import domain.model.{BUSD, FungibleData, TransactionHash, WBNB}
import domain.pricequote.{CurrencyPair, PriceQuote, PriceQuotes}
import vo.TimeInterval

import eu.timepit.refined.refineMV
import zio.test.Assertion._
import zio.test._

import java.time.Instant

object MarketPlaysSpec extends DefaultRunnableSpec {
  override def spec = suite("MarketPlays spec") {
    suite("Balance Trend") {
      test("Constant amount of coin with varying price quote in time interval") {
        val t: TopUp = TopUp(TxHash,
          value = FungibleData(amount = BigDecimal(5), WBNB),
          fee = FungibleData(amount = BigDecimal(1), WBNB),
          timestamp = Instant.parse("2021-10-08T13:00:00.000Z")
        )
        val marketPlays = MarketPlays(List(t))
        val interval = TimeInterval(Instant.parse("2021-10-06T17:00:00.000Z"), Instant.parse("2021-10-10T20:10:00.000Z"))
        val quotes = PriceQuotes(
          CurrencyPair(WBNB, BUSD),
          List(
            PriceQuote(304.20d, Instant.parse("2021-10-06T00:00:00.000Z")),
            PriceQuote(310d, Instant.parse("2021-10-07T00:00:00.000Z")),
            PriceQuote(308.7d, Instant.parse("2021-10-08T00:00:00.000Z")),
            PriceQuote(309d, Instant.parse("2021-10-09T00:00:00.000Z")),
            PriceQuote(306.24d, Instant.parse("2021-10-10T00:00:00.000Z"))
          ),
        )
        //(value - fee) * quote
        val expected = List(
          FungibleDataTimePoint(FungibleData.zero(BUSD), Instant.parse("2021-10-06T00:00:00.000Z")),
          FungibleDataTimePoint(FungibleData.zero(BUSD), Instant.parse("2021-10-07T00:00:00.000Z")),
          FungibleDataTimePoint(FungibleData(1234.8d, BUSD), Instant.parse("2021-10-08T00:00:00.000Z")),
          FungibleDataTimePoint(FungibleData(1236d, BUSD), Instant.parse("2021-10-09T00:00:00.000Z")),
          FungibleDataTimePoint(FungibleData(1224.96, BUSD), Instant.parse("2021-10-10T00:00:00.000Z")),
        )
        assert(marketPlays.balanceTrend(interval, quotes))(hasSameElements(expected))
      }
    }
  }

  private val TxHash: TransactionHash = refineMV("0xfaketxhash")
}
