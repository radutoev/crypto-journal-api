package io.softwarechain.cryptojournal
package infrastructure.api

import application.SyncApi
import domain.model.WalletAddressPredicate
import domain.wallet.error.InvalidWallet

import eu.timepit.refined.refineV
import zhttp.http._
import zio.ZIO

object Routes {
  val api = CORS(
    health +++ wallets
  )

  private lazy val health = HttpApp.collect {
    case Method.GET -> Root / "health" => Response.ok
  }

  private lazy val wallets = HttpApp.collectM {
    case Method.PUT -> Root / "wallets" / rawWalletAddress =>
      ZIO
        .fromEither(refineV[WalletAddressPredicate](rawWalletAddress.toLowerCase))
        .orElseFail(InvalidWallet(s"Invalid address $rawWalletAddress"))
        .flatMap(SyncApi.addWallet)
        .fold(
          _ => Response.status(Status.INTERNAL_SERVER_ERROR),
          _ => Response.status(Status.ACCEPTED)
        )
  }
}
