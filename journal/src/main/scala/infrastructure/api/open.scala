package io.softwarechain.cryptojournal
package infrastructure.api

import application.PositionHelper
import domain.model.{ Currency, TransactionHashPredicate, WalletAddressPredicate }
import domain.pricequote.CurrencyPair
import infrastructure.api.plays.dto.{ fromPositionEntry, fromPriceQuote }
import vo.TimeInterval

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.{ Chunk, ZIO }
import zio.json._

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
      (for {
        base  <- ZIO.fromOption(params.get("base").flatMap(_.headOption).map(Currency.unsafeFrom))
        quote <- ZIO.fromOption(params.get("quote").flatMap(_.headOption).map(Currency.unsafeFrom))
        start <- ZIO.fromOption(params.get("start").flatMap(_.headOption).map(Instant.parse(_)))
        end   <- ZIO.fromOption(params.get("end").flatMap(_.headOption).map(Instant.parse(_)))
        response <- PositionHelper
                     .quotes(CurrencyPair(base, quote), TimeInterval(start, end))
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
      } yield response).orElseFail(BadRequest("Invalid data"))
  }
}
