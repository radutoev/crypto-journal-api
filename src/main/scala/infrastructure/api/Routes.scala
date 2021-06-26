package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{UserId, WalletAddressPredicate}
import domain.portfolio.KpiService
import domain.position.PositionService
import domain.wallet.WalletService
import domain.wallet.error._
import infrastructure.api.dto.PortfolioKpi
import infrastructure.api.dto.PortfolioKpi._
import infrastructure.api.dto.Position._
import infrastructure.api.dto.Wallet._
import infrastructure.auth.JwtUserContext
import infrastructure.google.esp.AuthHeaderData
import infrastructure.google.esp.AuthHeaderData._
import vo.TimeInterval

import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio._
import zio.json._

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
                           Response.jsonString(positions.map(fromPosition).reverse.toJson)
                         } else {
                           Response.status(Status.NO_CONTENT)
                         }
                     )
      } yield response
  }

  private def portfolio(userId: UserId) = HttpApp.collectM {
    case Method.GET -> Root / "portfolio" / rawWalletAddress / "kpi" =>
      for {
        address <- ZIO
          .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
          .orElseFail(BadRequest("Invalid address"))

        response <- CryptoJournalApi
          .getPortfolioKpis(address, TimeInterval(Instant.now().minus(365, ChronoUnit.DAYS), Some(Instant.now())))
          .provideSomeLayer[Has[KpiService]](JwtUserContext.layer(userId))
          .fold(_ => Response.status(Status.INTERNAL_SERVER_ERROR), portfolioKpi => Response.jsonString(PortfolioKpi(portfolioKpi).toJson))
      } yield response
  }

  def authenticate[R, E](fail: HttpApp[R, E], success: UserId => HttpApp[R, E]): HttpApp[R, E] = Http.flatten {
    HttpApp.fromFunction { req =>
      req.getHeader(EspForwardedHeaderName).orElse(req.getHeader(EspForwardedHeaderName.toLowerCase))
        .flatMap(header => AuthHeaderData(header.value.toString).toOption.map(_.id).map(NonEmptyString.unsafeFrom))
        .fold[HttpApp[R, E]](fail)(success)
    }
  }
}
