package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ContextId, UserId, WalletAddressPredicate}
import domain.wallet.error._
import domain.wallet.{WalletService, Wallet => CJWallet}
import infrastructure.api.Routes.ApiError
import infrastructure.api.wallets.dto.Wallet.fromWallet
import infrastructure.auth.JwtRequestContext

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.{Chunk, Has, ZIO}

object wallets {
  def routes(userId: UserId, contextId: ContextId) = HttpApp.collectM {
    case Method.POST -> Root / "wallets" / rawWalletAddress =>
      ZIO
        .fromEither(refineV[WalletAddressPredicate](rawWalletAddress.toLowerCase))
        .orElseFail(InvalidWallet(s"Invalid address $rawWalletAddress"))
        .flatMap(address =>
          CryptoJournalApi
            .addWallet(address)
            .provideSomeLayer[Has[WalletService]](JwtRequestContext.layer(userId, contextId))
        )
        .fold(
          {
            case InvalidWallet(reason) =>
              Response.http(
                status = Status.BAD_REQUEST,
                headers = List(Header("Content-Type", "application/json")),
                content = ApiError(`type` = "InvalidInput", "Invalid addrress").toResponsePayload
              )
            case WalletAddressExists(address) => Response.status(Status.CONFLICT)
            case _                            => Response.status(Status.INTERNAL_SERVER_ERROR)
          },
          _ => Response.status(Status.CREATED)
        )

    case Method.DELETE -> Root / "wallets" / rawWalletAddress =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))
        response <- CryptoJournalApi
                     .removeWallet(address)
                     .provideSomeLayer[Has[WalletService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       _ => Response.status(Status.OK)
                     )
      } yield response

    case Method.GET -> Root / "wallets" =>
      CryptoJournalApi.getWallets
        .provideSomeLayer[Has[WalletService]](JwtRequestContext.layer(userId, contextId))
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR), {
            case Nil     => Response.status(Status.NO_CONTENT)
            case wallets => Response.jsonString(wallets.map(fromWallet).toJson)
          }
        )

    case Method.GET -> Root / "wallets" / rawWalletAddress / "import-status" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))
        response <- CryptoJournalApi
                     .getWalletImportState(address)
                     .fold(
                       walletsErrorToHttpResponse,
                       state =>
                         Response.http(
                           status = Status.OK,
                           content = HttpData.CompleteData(
                             Chunk.fromArray(state.toString.toJson.getBytes(HTTP_CHARSET))
                           )
                         )
                     )
      } yield response
  }

  val walletsErrorToHttpResponse: WalletError => UResponse = {
    case InvalidWallet(reason) =>
      Response.http(
        status = Status.BAD_REQUEST,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "InvalidInput", reason).toResponsePayload
      )
    case WalletAddressExists(address) =>
      Response.http(
        status = Status.CONFLICT,
        content = ApiError(`type` = "WalletAddressExists", s"Wallet ${address.value} already defined").toResponsePayload
      )
    case UnableToAddWallet(address) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "UnableToAddWallet").toResponsePayload
      )
    case UnableToRemoveWallet(address) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "UnableToRemoveWallet").toResponsePayload
      )
    case WalletNotFound(userId, address) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "WalletNotFound").toResponsePayload
      )
    case WalletFetchError(address, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "WalletFetchError").toResponsePayload
      )
    case WalletsFetchError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "WalletsFetchError").toResponsePayload
      )
  }

  object dto {
    final case class Wallet(userId: String, address: String)

    object Wallet {
      implicit val walletCodec: JsonCodec[Wallet] = DeriveJsonCodec.gen[Wallet]

      def fromWallet(wallet: CJWallet): Wallet =
        Wallet(wallet.userId.value, wallet.address.value)
    }
  }
}
