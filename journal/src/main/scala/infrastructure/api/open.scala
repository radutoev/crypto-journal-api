package io.softwarechain.cryptojournal
package infrastructure.api

import application.PositionHelper
import domain.model.{TransactionHashPredicate, WalletAddressPredicate}
import infrastructure.api.plays.dto.{fromPositionEntry, fromPriceQuote}

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.{Chunk, ZIO}

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

    case Method.GET -> Root / "test" / "bitquery" =>
      PositionHelper.bitqueryTest()
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
