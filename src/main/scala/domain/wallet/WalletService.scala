package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import domain.position.PositionService
import error.WalletError

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]
}

final case class LiveWalletService(walletRepo: WalletRepo,
                                   positionService: PositionService,
                                   logger: Logger[String]) extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    walletRepo.addWallet(userId, address)
      .zipRight(positionService.importPositions(userId, address).fork)
      .unit
  }

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = walletRepo.getWallets(userId)

  override def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    walletRepo.removeWallet(userId, address)
  }
}

object LiveWalletService {
  lazy val layer: URLayer[Has[WalletRepo] with Has[PositionService] with Logging, Has[WalletService]] =
    (LiveWalletService(_, _, _)).toLayer
}
