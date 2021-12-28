package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model._

import com.auth0.jwk.UrlJwkProvider
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import pdi.jwt.{Jwt, JwtAlgorithm}
import zhttp.http.{Header, _}
import zio._
import zio.json._

import java.util.UUID
import scala.util.Try

object Routes {
  private val forbidden = HttpApp.response(
    Response.http(
      status = Status.FORBIDDEN,
      headers = List(Header("Content-Type", "application/json")),
      content = ApiError(`type` = "Forbidden").toResponsePayload
    )
  )

  private val badRequest = HttpApp.response(
    Response.http(
      status = Status.BAD_REQUEST,
      headers = List(Header("Content-Type", "application/json")),
      content = ApiError(`type` = "BadRequest", "Invalid coin logger request id provided").toResponsePayload
    )
  )

  lazy val api = CORS(
    health +++
      open.routes +++
      authenticate(forbidden, userId => contextId(badRequest, cId => wallets.routes(userId, cId))) +++
      authenticate(forbidden, userId => contextId(badRequest, cId => plays.routes(userId, cId))) +++
      authenticate(forbidden, userId => contextId(badRequest, cId => portfolio.routes(userId, cId))) +++
      authenticate(forbidden, userId => contextId(badRequest, _ => market.routes(userId))) +++
      authenticate(forbidden, userId => contextId(badRequest, _ => setups.routes(userId))),
    config = CORSConfig(anyOrigin = true)
  )

  private def health = HttpApp.collect {
    case Method.GET -> Root / "health" => Response.ok
  }

  def authenticate[R, E](fail: HttpApp[R, E], success: UserId => HttpApp[R, E]): HttpApp[R, E] = Http.flatten {
//    HttpApp.fromFunction { req =>
//      req
//        .getHeader(EspForwardedHeaderName)
//        .orElse(req.getHeader(EspForwardedHeaderName.toLowerCase))
//        .flatMap(header => AuthHeaderData(header.value.toString).toOption.map(_.id).map(NonEmptyString.unsafeFrom))
//        .fold[HttpApp[R, E]](fail)(success)
//    }
    HttpApp.fromFunction { req =>
      req
        .getHeader("Authorization")
        .orElse(req.getHeader("authorization"))
        .flatMap(header => jwtDecode(header.value.toString.split("[ ]").last.trim).map(NonEmptyString.unsafeFrom))
        .fold[HttpApp[R, E]](fail)(success)
    }
  }

  def contextId[R, E](fail: HttpApp[R, E], success: ContextId => HttpApp[R, E]): HttpApp[R, E] = {
    @inline
    def makeContextId(rawClientId: String): Either[String, ContextId] =
      refineV[ContextIdPredicate](rawClientId.trim)

    Http.flatten {
      HttpApp.fromFunction { req =>
        req
          .getHeader("X-CoinLogger-ContextId")
          .orElse(req.getHeader("x-coinlogger-contextid"))
          .fold[Either[String, ContextId]](makeContextId(UUID.randomUUID().toString))(contextId =>
            makeContextId(contextId.value.toString.trim)
          )
          .fold(_ => fail, success)
      }
    }
  }

  private val domain   = "dev-13qiy-8m.eu.auth0.com"
  private val audience = "crypto-journal-api"

  // Helper to decode the JWT token
  def jwtDecode(token: String): Option[String] =
    Try(new UrlJwkProvider(s"https://$domain").get("whMXIhXP2wW3FeqmS8QX7"))
      .flatMap(jwk => Jwt.decode(token, jwk.getPublicKey, Seq(JwtAlgorithm.RS256)))
      .toOption
      .flatMap(_.content.fromJson[DecodedJwtClaims].toOption)
      .map(_.sub)

  final case class DecodedJwtClaims(
    given_name: String,
    family_name: Option[String],
    nickname: String,
    name: String,
    picture: String,
    locale: String,
    updated_at: String,
    email: String,
    email_verified: Boolean,
    iss: String,
    sub: String,
    aud: String,
    nonce: String
  )
  import zio.json.{DeriveJsonCodec, JsonCodec}
  object DecodedJwtClaims {
    implicit val jwtClaimsCodec: JsonCodec[DecodedJwtClaims] = DeriveJsonCodec.gen[DecodedJwtClaims]
  }


  //TODO Maybe add a throwable for easier debugging.
  final case class ApiError(`type`: String, details: Option[String])

  object ApiError {
    implicit val apiErrorCodec: JsonCodec[ApiError] = DeriveJsonCodec.gen[ApiError]

    def apply(`type`: String, details: String) = new ApiError(`type`, Some(details))

    def apply(`type`: String) = new ApiError(`type`, None)

    implicit class ApiErrorOps(apiError: ApiError) {
      def toResponsePayload: HttpData.CompleteData =
        HttpData.CompleteData(Chunk.fromArray(apiError.toJson.getBytes(HTTP_CHARSET)))
    }
  }
}
