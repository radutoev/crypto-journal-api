package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{UserId, WalletAddressPredicate}
import domain.portfolio.KpiService
import domain.position.error._
import domain.position.Position.PositionIdPredicate
import domain.position.{JournalingService, PositionService}
import domain.wallet.WalletService
import domain.wallet.error._
import infrastructure.api.dto.PortfolioKpi
import infrastructure.api.dto.PortfolioKpi._
import infrastructure.api.dto.Position._
import infrastructure.api.dto.Wallet._
import infrastructure.api.dto.JournalEntry
import infrastructure.api.dto.JournalEntry._
import infrastructure.auth.JwtUserContext
import infrastructure.google.esp.AuthHeaderData
import infrastructure.google.esp.AuthHeaderData._
import vo.{PositionFilter, TimeInterval}

import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio._
import zio.json._
import zio.prelude.Validation

import java.time.Instant
import java.time.temporal.ChronoUnit

object Routes {
  val api = CORS(
    health +++
      authenticate(HttpApp.forbidden("Not allowed!"), wallets) +++
      authenticate(HttpApp.forbidden("Not allowed!"), positions) +++
      authenticate(HttpApp.forbidden("Not allowed!"), portfolio),
    config = CORSConfig(anyOrigin = true)
  )

  private def health = HttpApp.collect {
    case Method.GET -> Root / "health" => Response.ok
  }

  private def wallets(userId: UserId) = HttpApp.collectM {
    case Method.POST -> Root / "wallets" / rawWalletAddress =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress.toLowerCase))
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
    case req @ Method.GET -> Root / "addresses" / rawWalletAddress / "positions" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        filter <- req.url.positionFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPositions(address)
                     .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       positions =>
                         if (positions.items.nonEmpty) {
                           if (positions.lastSync.isDefined) {
                             Response.http(
                               status = Status.OK,
                               headers = List(
                                 Header("X-CoinLogger-LatestSync", positions.lastSync.get.toString),
                                 Header("Content-Type", "application/json")
                               ),
                               content = HttpData.CompleteData(
                                 Chunk
                                   .fromArray(positions.items.map(fromPosition).reverse.toJson.getBytes(HTTP_CHARSET))
                               )
                             )
                           } else {
                             Response.jsonString(positions.items.map(fromPosition).reverse.toJson)
                           }
                         } else {
                           if (positions.lastSync.isDefined) {
                             Response.http(
                               status = Status.NO_CONTENT,
                               headers = List(
                                 Header("X-CoinLogger-LatestSync", positions.lastSync.get.toString),
                                 Header("Content-Type", "application/json")
                               )
                             )
                           } else {
                             Response.status(Status.NO_CONTENT)
                           }
                         }
                     )
      } yield response

    case Method.GET ->  Root / "addresses" / rawWalletAddress / "positions" / "diff" =>
      for {
        address <- ZIO
          .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
          .orElseFail(BadRequest("Invalid address"))

        response <- CryptoJournalApi
          .diff(address)
          .provideSomeLayer[Has[PositionService]](JwtUserContext.layer(userId))
          .fold(
            {
              case _: CheckpointNotFound => Response.status(Status.NOT_FOUND)
            },
            positions => if(positions.items.nonEmpty) {
              positions.lastSync match {
                case None => Response.jsonString(positions.items.map(fromPosition).reverse.toJson)
                case Some(timestamp) =>
                  Response.http(
                    status = Status.OK,
                    headers = List(
                      Header("X-CoinLogger-LatestSync", timestamp.toString),
                      Header("Content-Type", "application/json")
                    ),
                    content = HttpData.CompleteData(
                      Chunk.fromArray(positions.items.map(fromPosition).reverse.toJson.getBytes(HTTP_CHARSET))
                    )
                  )
              }
            } else {
              positions.lastSync match {
                case None => Response.jsonString(positions.items.map(fromPosition).reverse.toJson)
                case Some(timestamp) =>
                  Response.http(
                    status = Status.NO_CONTENT,
                    headers = List(
                      Header("X-CoinLogger-LatestSync", timestamp.toString),
                      Header("Content-Type", "application/json")
                    )
                  )
              }
              Response.status(Status.NO_CONTENT)
            }
          )
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
                       {
                         case PositionNotFound(_) =>
                           Response.status(Status.NOT_FOUND)
                         case _ => Response.status(Status.INTERNAL_SERVER_ERROR)
                       },
                       position => Response.jsonString(fromJournalPosition(position).toJson)
                     )
      } yield response

    case req@Method.PUT -> Root / "positions" / rawPositionId / "journal" =>
      for {
        positionId <- ZIO
          .fromEither(refineV[PositionIdPredicate](rawPositionId))
          .orElseFail(BadRequest("Invalid positionId"))

        journalEntry <- ZIO.fromOption(req.getBodyAsString)
          .flatMap(rawBody => ZIO.fromEither(rawBody.fromJson[JournalEntry]))
          .orElseFail(BadRequest("Invalid request"))

        response <- CryptoJournalApi.saveJournalEntry(positionId, journalEntry.toDomainModel)
          .provideSomeLayer[Has[JournalingService]](JwtUserContext.layer(userId))
          .fold(_ => Response.status(Status.INTERNAL_SERVER_ERROR), _ => Response.status(Status.OK))
      } yield response
  }

  private def portfolio(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "portfolio" / rawWalletAddress / "kpi" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        response <- CryptoJournalApi
                     .getPortfolioKpis(
                       address,
                       TimeInterval(Instant.now().minus(365, ChronoUnit.DAYS), Some(Instant.now()))
                     )
                     .provideSomeLayer[Has[KpiService]](JwtUserContext.layer(userId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi => Response.jsonString(PortfolioKpi(portfolioKpi).toJson)
                     )
      } yield response
  }

  def authenticate[R, E](fail: HttpApp[R, E], success: UserId => HttpApp[R, E]): HttpApp[R, E] = Http.flatten {
    HttpApp.fromFunction { req =>
      req
        .getHeader(EspForwardedHeaderName)
        .orElse(req.getHeader(EspForwardedHeaderName.toLowerCase))
        .flatMap(header => AuthHeaderData(header.value.toString).toOption.map(_.id).map(NonEmptyString.unsafeFrom))
        .fold[HttpApp[R, E]](fail)(success)
    }
  }

  implicit class PositionsQParamsOps(url: URL) {
    def positionFilter(): Validation[String, PositionFilter] = {
      val qParams = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }
      getInt("count", 30)(qParams).flatMap(count => PositionFilter(count))
    }

    private def getInt(key: String, default: Int)(qParams: Map[String, String]): Validation[String, Int] = {
      if(qParams.contains(key)) {
        Validation.fromOption(qParams(key).toIntOption).mapError(_ => "Query param has to be an integer")
      } else {
        Validation.succeed(default)
      }
    }
  }
}
