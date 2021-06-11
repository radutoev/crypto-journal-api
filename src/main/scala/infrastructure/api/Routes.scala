package io.softwarechain.cryptojournal
package infrastructure.api

import domain.position.PositionRepo
import infrastructure.api.model.Position.fromPosition

import zhttp.http._
import zio.UIO
import zio.json._

object Routes {
  val api = Http.collectM[Request] {
    case Method.GET -> Root / "health" => UIO(Response.ok)

    case Method.GET -> Root / "positions" =>
      PositionRepo.getPositions("0x627909aDAb1AB107b59A22e7ddd15e5d9029bC41")
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR),
          positions => Response.jsonString(positions.map(fromPosition).toJson)
        )
  }
}
