package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ UserId, WalletAddressPredicate }
import domain.portfolio.KpiService
import domain.position.Position.PositionIdPredicate
import domain.position.error._
import domain.position.{ JournalPositions, JournalingService, PositionService, Positions }
import domain.wallet.WalletService
import domain.wallet.error._
import infrastructure.api.dto.JournalEntry._
import infrastructure.api.dto.PortfolioKpi._
import infrastructure.api.dto.{ JournalEntry, PortfolioKpi, PortfolioStats, PositionJournalEntry }
import infrastructure.api.dto.PortfolioStats._
import infrastructure.api.dto.Position._
import infrastructure.api.dto.PositionJournalEntry._
import infrastructure.api.dto.Wallet._
import infrastructure.auth.JwtUserContext
import infrastructure.google.esp.AuthHeaderData
import infrastructure.google.esp.AuthHeaderData._
import vo.TimeInterval
import vo.filter.{ KpiFilter, PositionCount, PositionFilter }

import com.auth0.jwk.UrlJwkProvider
import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import pdi.jwt.{ Jwt, JwtAlgorithm, JwtClaim }
import zhttp.http.HttpError.BadRequest
import zhttp.http.{ Header, _ }
import zio._
import zio.json._
import zio.prelude._

import java.time.{ LocalDate, ZoneId, ZoneOffset }
import scala.util.Try

object Routes {
  private val forbidden = HttpApp.response(
    Response.http(
      status = Status.FORBIDDEN,
      headers = List(Header("Content-Type", "application/json")),
      content = ApiError(`type` = "Forbidden").toResponsePayload()
    )
  )

  val api = CORS(
    health +++
      authenticate(forbidden, wallets) +++
      authenticate(forbidden, positions) +++
      authenticate(forbidden, portfolio) +++
      authenticate(forbidden, setups) +++
      authenticate(forbidden, mistakes),
    config = CORSConfig(anyOrigin = true)
  )

  private def health = HttpApp.collect {
    case Method.GET -> Root / "health" => Response.ok
  }

  private def wallets(userId: UserId) = HttpApp.collectM {
    case Method.POST -> Root / "wallets" / rawWalletAddress => {
      ZIO
        .fromEither(refineV[WalletAddressPredicate](rawWalletAddress.toLowerCase))
        .orElseFail(InvalidWallet(s"Invalid address ${rawWalletAddress}"))
        .flatMap(address =>
          CryptoJournalApi
            .addWallet(address)
            .provideSomeLayer[Has[WalletService]](JwtUserContext.layer(userId))
        )
        .fold(
          {
            case InvalidWallet(reason) =>
              Response.http(
                status = Status.BAD_REQUEST,
                headers = List(Header("Content-Type", "application/json")),
                content = ApiError(`type` = "InvalidInput", "Invalid addrress").toResponsePayload()
              )
            case WalletAddressExists(address) => Response.status(Status.CONFLICT)
            case _                            => Response.status(Status.INTERNAL_SERVER_ERROR)
          },
          _ => Response.status(Status.CREATED)
        )
    }

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
    case req @ Method.GET -> Root / "addresses" / rawWalletAddress / "positions" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        filter <- req.url.positionFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPositions(address, filter)
                     .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
                     .fold(positionErrorToHttpResponse, _.asResponse())
      } yield response

    case Method.GET -> Root / "addresses" / rawWalletAddress / "positions" / "diff" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        response <- CryptoJournalApi
                     .diff(address)
                     .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
                     .fold(positionErrorToHttpResponse, _.asResponse())
      } yield response

    case Method.GET -> Root / "positions" / rawPositionId =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PositionIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        response <- CryptoJournalApi
                     .getPosition(positionId)
                     .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
                     .fold(
                       positionErrorToHttpResponse,
                       position => Response.jsonString(fromJournalPosition(position).toJson)
                     )
      } yield response

    case req @ Method.PUT -> Root / "positions" / rawPositionId / "journal" =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PositionIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        journalEntry <- ZIO
                         .fromOption(req.getBodyAsString)
                         .flatMap(rawBody => ZIO.fromEither(rawBody.fromJson[JournalEntry]))
                         .orElseFail(BadRequest("Invalid request"))

        response <- CryptoJournalApi
                     .saveJournalEntry(positionId, journalEntry.toDomainModel)
                     .provideSomeLayer[Has[JournalingService]](JwtUserContext.layer(userId))
                     .fold(positionErrorToHttpResponse, _ => Response.status(Status.OK))
      } yield response

    case req @ Method.PUT -> Root / "journal" =>
      for {
        entries <- ZIO
                    .fromOption(req.getBodyAsString)
                    .flatMap(rawBody =>
                      ZIO.fromEither(rawBody.fromJson[List[PositionJournalEntry]]).map(_.map(_.toDomainModel))
                    )
                    .orElseFail(BadRequest("Invalid request"))
        response <- CryptoJournalApi
                     .saveJournalEntries(entries)
                     .provideSomeLayer[Has[JournalingService]](JwtUserContext.layer(userId))
                     .fold(positionErrorToHttpResponse, _ => Response.status(Status.OK))
      } yield response
  }

  private def portfolio(userId: UserId) = HttpApp.collectM {
    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "kpi" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtUserContext.layer(userId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi => Response.jsonString(PortfolioKpi(portfolioKpi).toJson)
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "stats" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtUserContext.layer(userId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi => Response.jsonString(PortfolioStats(portfolioKpi).toJson)
                     )
      } yield response
  }

  private def setups(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "setups" => UIO(Response.jsonString(List("Presale", "Fair Launch").toJson))
  }

  private def mistakes(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "mistakes" => UIO(Response.jsonString(List("Honeypot", "Sold to early").toJson))
  }

  private def corsSupport() = HttpApp.collect {
    case Method.OPTIONS -> Root / "wallets"                                             => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "wallets" / rawWalletAddress                          => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "addresses" / rawWalletAddress / "positions"          => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "addresses" / rawWalletAddress / "positions" / "diff" => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "positions" / rawPositionId / "journal"               => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "positions" / rawPositionId                           => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "portfolio" / rawWalletAddress / "kpi"                => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "portfolio" / rawWalletAddress / "stats"              => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "mistakes"                                            => allowCorsRequestsResponse
    case Method.OPTIONS -> Root / "setups"                                              => allowCorsRequestsResponse
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
  import zio.json.{ DeriveJsonCodec, JsonCodec }
  object DecodedJwtClaims {
    implicit val jwtClaimsCodec: JsonCodec[DecodedJwtClaims] = DeriveJsonCodec.gen[DecodedJwtClaims]
  }

  trait QParamsOps {
    def getInt(key: String, default: Int)(qParams: Map[String, String]): Validation[String, Int] =
      if (qParams.contains(key)) {
        Validation.fromOption(qParams(key).toIntOption).mapError(_ => "Query param has to be an integer")
      } else {
        Validation.succeed(default)
      }
  }

  implicit class PositionsQParamsOps(url: URL) extends QParamsOps {
    def positionFilter(): Validation[String, PositionFilter] = {
      val qParams = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }

      Validation.validateWith(
        getInt("count", 30)(qParams).flatMap(cnt => PositionCount.make(cnt)),
        url.intervalFilter()
      ) {
        case (count, interval) =>
          new PositionFilter(count, interval)
      }
    }
  }

  implicit class KpiQParamsOps(url: URL) {
    def kpiFilter(): Validation[String, KpiFilter] =
      url.intervalFilter().map(KpiFilter)
  }

  implicit class IntervalQParamsOps(url: URL) {
    def intervalFilter(): Validation[String, TimeInterval] = {
      val qParams      = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }
      val rawStartDate = qParams.getOrElse("startdate", "")
      val rawEndDate   = qParams.getOrElse("enddate", "")

      try {
        val start = LocalDate.parse(rawStartDate).atStartOfDay(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        val end   = LocalDate.parse(rawEndDate).atStartOfDay(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        Validation.succeed(TimeInterval(start, end))
      } catch {
        case _: Exception => Validation.fail("Invalid time interval")
      }
    }
  }

  implicit class PositionsResponseOps(positions: Positions) {
    def asResponse(): UResponse = {
      val resultHeaders: List[Header] = positions.lastSync match {
        case Some(value) => List(Header("X-CoinLogger-LatestSync", value.toString))
        case None        => Nil
      }
      val headers = Header("Content-Type", "application/json") :: resultHeaders

      positions.items match {
        case list =>
          Response.http(
            status = Status.OK,
            headers = headers,
            content = HttpData.CompleteData(
              Chunk.fromArray(list.map(fromPosition).toJson.getBytes(HTTP_CHARSET))
            )
          )
        case Nil => Response.http(status = Status.NO_CONTENT, headers = headers)
      }
    }
  }

  implicit class JournalPositionsResponseOps(positions: JournalPositions) {
    def asResponse(): UResponse = {
      val resultHeaders: List[Header] = positions.lastSync match {
        case Some(value) => List(Header("X-CoinLogger-LatestSync", value.toString))
        case None        => Nil
      }
      val headers = Header("Content-Type", "application/json") :: resultHeaders

      positions.items match {
        case list =>
          Response.http(
            status = Status.OK,
            headers = headers,
            content = HttpData.CompleteData(
              Chunk.fromArray(list.map(fromJournalPosition).toJson.getBytes(HTTP_CHARSET))
            )
          )
        case Nil => Response.http(status = Status.NO_CONTENT, headers = headers)
      }
    }
  }

  val positionErrorToHttpResponse: PositionError => UResponse = {
    case InvalidRepresentation(message) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "InvalidRepresentation", message).toResponsePayload()
      )
    case PositionsFetchError(address) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionsFetchError", s"Error retrieving positions for address: $address")
          .toResponsePayload()
      )
    case PositionNotFound(_) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionNotFound").toResponsePayload()
      )
    case PositionFetchError(positionId, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionFetchError", s"Error retrieving position $positionId").toResponsePayload()
      )
    case PriceQuotesError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content =
          ApiError(`type` = "PriceQuotesError", s"Failure applying price quotes onp positions").toResponsePayload()
      )
    case CheckpointFetchError(address, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "CheckpointFetchError").toResponsePayload()
      )
    case CheckpointNotFound(address) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = s"No checkpoint for address ${address}").toResponsePayload()
      )
    case JournalSaveError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalSaveError").toResponsePayload()
      )
    case JournalFetchError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalFetchError").toResponsePayload()
      )
    case JournalNotFound(userId, positionId) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalNotFound").toResponsePayload()
      )
    case PositionImportError(address, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionImportError").toResponsePayload()
      )
    case InvalidInput(reason) =>
      Response.http(
        status = Status.BAD_REQUEST,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "InvalidInput", reason).toResponsePayload()
      )
  }

  //TODO Maybe add a throwable for easier debugging.
  final case class ApiError(`type`: String, details: Option[String])

  object ApiError {
    implicit val apiErrorCodec: JsonCodec[ApiError] = DeriveJsonCodec.gen[ApiError]

    def apply(`type`: String, details: String) = new ApiError(`type`, Some(details))

    def apply(`type`: String) = new ApiError(`type`, None)

    implicit class ApiErrorOps(apiError: ApiError) {
      def toResponsePayload() = HttpData.CompleteData(Chunk.fromArray(apiError.toJson.getBytes(HTTP_CHARSET)))
    }
  }

  private val allowCorsRequestsResponse = Response.http(
    status = Status.OK,
    headers = List(
      Header("Access-Control-Allow-Origin", "*"),
      Header("Access-Control-Allow-Methods", "*"),
      Header("Access-Control-Allow-Headers", "*"),
      Header("Access-Control-Max-Age", "3600")
    )
  )
}
