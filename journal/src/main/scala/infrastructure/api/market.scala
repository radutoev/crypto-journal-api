package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.market.{Ohlcv => CJOhlcv}
import domain.model.UserId
import infrastructure.api.common.dto.FungibleData
import infrastructure.api.common.dto._

import zhttp.http._
import zio.json._

import java.time.Instant

object market {
  def routes(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "markets" / "ohlcv" =>
      for {
        response <- CryptoJournalApi.getHistoricalOhlcv
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       data => Response.jsonString(data.map(o => Ohlcv(o)).toJson)
                     )
      } yield response
  }

  final case class Ohlcv(
                          timePeriodStart: Instant,
                          timePeriodEnd: Instant,
                          timeOpen: Instant,
                          timeClose: Instant,
                          priceOpen: FungibleData,
                          priceHigh: FungibleData,
                          priceLow: FungibleData,
                          priceClose: FungibleData
                        )

  object Ohlcv {
    implicit val ohlcvEntryCodec: JsonCodec[Ohlcv] = DeriveJsonCodec.gen[Ohlcv]
    def apply(ohlcv: CJOhlcv): Ohlcv =
      new Ohlcv(
        ohlcv.timePeriodStart,
        ohlcv.timePeriodEnd,
        ohlcv.timeOpen,
        ohlcv.timeClose,
        ohlcv.priceOpen.asJson,
        ohlcv.priceHigh.asJson,
        ohlcv.priceLow.asJson,
        ohlcv.priceClose.asJson
      )
  }
}
