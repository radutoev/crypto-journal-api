package io.softwarechain.cryptojournal
package infrastructure.api

import application.PositionHelper
import domain.model.date.{DayUnit, HourUnit, MinuteUnit}
import domain.model.{CurrencyPredicate, TransactionHashPredicate, WalletAddressPredicate}
import domain.pricequote.CurrencyPair
import infrastructure.api.plays.dto.{fromPositionEntry, fromPriceQuote}
import vo.TimeInterval

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.{Chunk, UIO, ZIO}

import java.time.Instant

object open {
  lazy val routes = HttpApp.collectM {
    case Method.GET -> Root / "test" / "address" / rawWalletAddress / "tx2entries" / rawTxHash =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))
        txHash <- ZIO.fromEither(refineV[TransactionHashPredicate](rawTxHash)).orElseFail(BadRequest("Invalid tx hash"))
        response <- PositionHelper
                     .txToEntries(address, txHash)
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR), {
                         case Nil => Response.http(status = Status.NO_CONTENT)
                         case list =>
                           Response.http(
                             status = Status.OK,
                             headers = Header("Content-Type", "application/json") :: Nil,
                             content = HttpData.CompleteData(
                               Chunk.fromArray(list.map(fromPositionEntry).toJson.getBytes(HTTP_CHARSET))
                             )
                           )
                       }
                     )
      } yield response

    case req @ Method.GET -> Root / "test" / "quotes" =>
      val params = req.url.queryParams
      val pair = CurrencyPair(
        refineV[CurrencyPredicate](params("base").head).right.get,
        refineV[CurrencyPredicate](params("quote").head).right.get
      )
      val interval = TimeInterval(Instant.parse(params("start").head), Instant.parse(params("end").head))
      val unit = params("unit").head match {
        case "day"    => DayUnit
        case "hour"   => HourUnit
        case "minute" => MinuteUnit
        case _        => DayUnit
      }

      PositionHelper
        .quotesTest(pair, interval, unit)
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR), {
            case Nil => Response.http(status = Status.NO_CONTENT)
            case list =>
              Response.http(
                status = Status.OK,
                headers = Header("Content-Type", "application/json") :: Nil,
                content = HttpData.CompleteData(
                  Chunk.fromArray(list.map(fromPriceQuote).toJson.getBytes(HTTP_CHARSET))
                )
              )
          }
        )
  }
}
