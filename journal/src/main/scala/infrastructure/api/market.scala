package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ PlayIdPredicate, UserId, Ohlcv => CJOhlcv }

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.ZIO
import zio.json._

import java.time.Instant

object market {
  def routes(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "positions" / rawPositionId / "ohlcv" =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PlayIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))
        response <- CryptoJournalApi
                     .getHistoricalOhlcv(positionId)
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       data => Response.jsonString(data.map(o => Ohlcv(o)).toJson)
                     )
      } yield response
  }

  final case class Ohlcv(
    timestamp: Instant,
    open: BigDecimal,
    close: BigDecimal,
    minimum: BigDecimal,
    median: BigDecimal,
    maximum: BigDecimal,
    volume: BigDecimal
  )

  object Ohlcv {
    implicit val ohlcvEntryCodec: JsonCodec[Ohlcv] = DeriveJsonCodec.gen[Ohlcv]
    def apply(ohlcv: CJOhlcv): Ohlcv =
      new Ohlcv(
        ohlcv.timestamp,
        ohlcv.open,
        ohlcv.close,
        ohlcv.minimum,
        ohlcv.median,
        ohlcv.maximum,
        ohlcv.volume
      )
  }
}
