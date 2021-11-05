package io.softwarechain.cryptojournal
package infrastructure.coinapi

import domain.market.Ohlcv

import io.softwarechain.cryptojournal.domain.model.{Currency, FungibleData}
import zio.json.{DeriveJsonDecoder, JsonDecoder, jsonField}

import java.time.Instant

object dto {
  final case class HistoricalData(
    @jsonField("time_period_start") timePeriodStart: String,
    @jsonField("time_period_end") timePeriodEnd: String,
    @jsonField("time_open") timeOpen: String,
    @jsonField("time_close") timeClose: String,
    @jsonField("price_open") priceOpen: Double,
    @jsonField("price_high") priceHigh: Double,
    @jsonField("price_low") priceLow: Double,
    @jsonField("price_close") priceClose: Double,
    @jsonField("volume_traded") volumeTraded: Double,
    @jsonField("trades_count") tradesCount: Long
  )

  object HistoricalData {
    implicit val decoder: JsonDecoder[HistoricalData] = DeriveJsonDecoder.gen[HistoricalData]

    def toDomain(data: HistoricalData): Ohlcv = {
      val usd = Currency.unsafeFrom("USD")
      Ohlcv(
        Instant.parse(data.timePeriodStart),
        Instant.parse(data.timePeriodEnd),
        Instant.parse(data.timeOpen),
        Instant.parse(data.timeClose),
        FungibleData(data.priceOpen, usd),
        FungibleData(data.priceHigh, usd),
        FungibleData(data.priceLow, usd),
        FungibleData(data.priceClose, usd)
      )
    }
  }
}
