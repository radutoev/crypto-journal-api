package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import error.WalletError

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]
}

final case class LiveWalletService(walletRepo: WalletRepo, logger: Logger[String]) extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    walletRepo.addWallet(userId, address)
  }

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = walletRepo.getWallets(userId)
}

object LiveWalletService {
  lazy val layer: URLayer[Has[WalletRepo] with Logging, Has[WalletService]] =
    (LiveWalletService(_, _)).toLayer
}
