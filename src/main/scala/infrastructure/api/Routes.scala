package io.softwarechain.cryptojournal
package infrastructure.api

import infrastructure.covalent.BlockchainQuery

import zio.json._
import zhttp.http._
import zio.UIO

object Routes {
  val api = Http.collectM[Request] {
    case Method.GET -> Root / "health" => UIO(Response.ok)

    case Method.GET -> Root / "hello" =>
      BlockchainQuery.fetchTransactions("0x627909aDAb1AB107b59A22e7ddd15e5d9029bC41")
        .fold(_ => Response.status(Status.INTERNAL_SERVER_ERROR), transactions => Response.jsonString(transactions.toJson))
  }
}
