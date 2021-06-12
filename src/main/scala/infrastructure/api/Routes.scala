package io.softwarechain.cryptojournal
package infrastructure.api

import domain.position.PositionRepo
import infrastructure.api.dto.Position.fromPosition

import zhttp.http._
import zio.UIO
import zio.json._

object Routes {
  val api = CORS(
    Http.collectM[Request] {
      case Method.GET -> Root / "health" => UIO(Response.ok)

      case Method.GET -> Root / "positions" / rawWalletAddress =>
        PositionRepo.getPositions(rawWalletAddress)
          .fold(
            _ => Response.status(Status.INTERNAL_SERVER_ERROR),
            positions => Response.jsonString(positions.map(fromPosition).toJson)
          )
    },
    config = CORSConfig(anyOrigin = true)
  )
}
