package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import domain.error.WalletError

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]
}

final case class LiveWalletService(walletRepo: WalletRepo, logger: Logger[String]) extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    walletRepo.addWallet(userId, address)
  }
}

object LiveWalletService {
  lazy val layer: URLayer[Has[WalletRepo] with Logging, Has[WalletService]] =
    (LiveWalletService(_, _)).toLayer
}
