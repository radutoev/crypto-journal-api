package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ UserId, WalletAddressPredicate }
import domain.wallet.error._
import domain.wallet.WalletService
import domain.position.PositionService
import infrastructure.api.dto.Position._
import infrastructure.api.dto.Wallet._
import infrastructure.auth.JwtUserContext

import eu.timepit.refined.types.string.NonEmptyString
import eu.timepit.refined.refineV
import pdi.jwt.{ Jwt, JwtClaim }
import zhttp.http.HttpError.BadRequest
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

  private def wallets(userId: UserId) = HttpApp.collectM {
    case Method.POST -> Root / "wallets" / rawWalletAddress =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))
        response <- CryptoJournalApi
                     .addWallet(address)
                     .provideSomeLayer[Has[WalletService]](JwtUserContext.layer(userId))
                     .fold(
                       {
                         case WalletAddressExists(address) => Response.status(Status.CONFLICT)
                         case _                            => Response.status(Status.INTERNAL_SERVER_ERROR)
                       },
                       _ => Response.status(Status.CREATED)
                     )
      } yield response

    case Method.DELETE -> Root / "wallets" / rawWalletAddress =>
      for {
        address <- ZIO
          .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
          .orElseFail(BadRequest("Invalid address"))
        response <- CryptoJournalApi
          .removeWallet(address)
          .provideSomeLayer[Has[WalletService]](JwtUserContext.layer(userId))
          .fold(
            _ => Response.status(Status.INTERNAL_SERVER_ERROR),
            _ => Response.status(Status.OK)
          )
      } yield response

    case Method.GET -> Root / "wallets" =>
      CryptoJournalApi
        .getWallets()
        .provideSomeLayer[Has[WalletService]](JwtUserContext.layer(userId))
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR), {
            case Nil     => Response.status(Status.NO_CONTENT)
            case wallets => Response.jsonString(wallets.map(fromWallet).toJson)
          }
        )
  }

  private def positions(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "positions" / rawWalletAddress =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        response <- CryptoJournalApi
                     .getPositions(address)
                     .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       positions =>
                         if (positions.nonEmpty) {
                           Response.jsonString(positions.map(fromPosition).toJson)
                         } else {
                           Response.status(Status.NO_CONTENT)
                         }
                     )
      } yield response
  }

  def authenticate[R, E](fail: HttpApp[R, E], success: UserId => HttpApp[R, E]): HttpApp[R, E] =
    HttpApp.fromFunction {
      _.getHeader("Authorization")
      //        .flatMap(header => Some(header.value.toString.split("[ ]").last))
      //        .flatMap(jwtDecode) //TODO I need a function that can decode the AUTH0 token
        .flatMap(_ => Some(NonEmptyString.unsafeFrom("abcdef")))
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
