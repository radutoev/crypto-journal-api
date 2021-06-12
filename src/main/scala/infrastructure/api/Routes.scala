package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import infrastructure.api.dto.Position._

import zhttp.http._
import zio.UIO
import zio.json._

object Routes {
  val api = CORS(
    Http.collectM[Request] {
      case Method.GET -> Root / "health" => UIO(Response.ok)

      case Method.GET -> Root / "positions" / rawWalletAddress =>
        CryptoJournalApi
          .getCryptoFiatPositions(rawWalletAddress)
          .fold(
            _ => Response.status(Status.INTERNAL_SERVER_ERROR),
            positions =>
              if (positions.nonEmpty) {
                Response.jsonString(positions.map(fromPosition).toJson)
              } else {
                Response.status(Status.NO_CONTENT)
              }
          )
    },
    config = CORSConfig(anyOrigin = true)
  )
}
