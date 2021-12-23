package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.UserId

import zhttp.http._
import zio.UIO
import zio.json._

object setups {
  def routes(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "tags"     => UIO(Response.jsonString(List("Presale", "Fair Launch").toJson))
    case Method.GET -> Root / "mistakes" => UIO(Response.jsonString(List("Honeypot", "Sold to early").toJson))
  }
}
