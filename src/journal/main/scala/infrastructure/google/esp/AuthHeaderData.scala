package io.softwarechain.cryptojournal
package infrastructure.google.esp

import zio.json.{DecoderOps, DeriveJsonDecoder, JsonDecoder}

import java.nio.charset.StandardCharsets
import java.util.Base64
import scala.util.Try

/**
 * Data forwarded from Endpoints to service
 *
 * @see https://cloud.google.com/endpoints/docs/openapi/authenticating-users-auth0?authuser=2
 */
//TODO There is also a `claims` property; might need it in the future
case class AuthHeaderData(id: String, issuer: Option[String], email: Option[String], audiences: List[String])

object AuthHeaderData {
  implicit val authHeaderDataCode: JsonDecoder[AuthHeaderData] = DeriveJsonDecoder.gen[AuthHeaderData]

  val EspForwardedHeaderName: String = "X-Endpoint-API-UserInfo"

  /**
   * @param encoded base64 encoded data.
   * @return
   */
  def apply(encoded: String): Either[String, AuthHeaderData] =
    Try(new String(Base64.getDecoder.decode(encoded), StandardCharsets.UTF_8)).toEither.left
      .map(_ => "Unable to decode token data")
      .flatMap(string => string.fromJson[AuthHeaderData])
}
