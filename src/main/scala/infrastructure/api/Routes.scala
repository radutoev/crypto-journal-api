package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import infrastructure.api.dto.Position._

import pdi.jwt.{ Jwt, JwtClaim }
import zhttp.http._
import zio._
import zio.json._

object Routes {
  val api = CORS(
    health +++
      authenticate(HttpApp.forbidden("Not allowed!"), wallets) +++
      authenticate(HttpApp.forbidden("Not allowed!"), positions),
    config = CORSConfig(anyOrigin = true)
  )

  private def health = HttpApp.collect {
    case Method.GET -> Root / "health" => Response.ok
  }

  private def wallets(claim: JwtClaim) = HttpApp.collectM {
    case Method.POST -> Root / "wallets" / rawWalletAddress => UIO(Response.text(claim.content))
  }

  private def positions(claim: JwtClaim) = HttpApp.collectM {
    case Method.GET -> Root / "positions" / rawWalletAddress => {
      CryptoJournalApi
        .getPositions(rawWalletAddress)
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR),
          positions =>
            if (positions.nonEmpty) {
              Response.jsonString(positions.map(fromPosition).toJson)
            } else {
              Response.status(Status.NO_CONTENT)
            }
        )
    }
  }

  def authenticate[R, E](fail: HttpApp[R, E], success: JwtClaim => HttpApp[R, E]): HttpApp[R, E] =
    HttpApp.fromFunction {
      _.getHeader("Authorization")
        .flatMap(header => jwtDecode(header.value.toString))
        .fold[HttpApp[R, E]](fail)(success)
    }

  // Helper to decode the JWT token
  private def jwtDecode(token: String): Option[JwtClaim] =
    //Jwt.decode(token, SECRET_KEY, Seq(JwtAlgorithm.HS512)).toOption
    Jwt.decode(token).toOption

  //  implicit class ListResponseOps[T](list: List[T]) {
//    def asResponse: UResponse = list match {
//      case Nil => Response.status(Status.NO_CONTENT)
//      case list => Response.jsonString("test") //TODO I need to have a json representation trait or something
//    }
//  }
}
