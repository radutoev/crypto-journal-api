package io.softwarechain.cryptojournal
package domain.wallet

import error.WalletError
import io.softwarechain.cryptojournal.domain.model.{ UserId, WalletAddress }
import zio.{ Has, IO, ZIO }

trait UserWalletRepo {
  def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]
}

object UserWalletRepo {
  def addWallet(userId: UserId, address: WalletAddress): ZIO[Has[UserWalletRepo], WalletError, Unit] =
    ZIO.serviceWith[UserWalletRepo](_.addWallet(userId, address))

  def getWallets(userId: UserId): ZIO[Has[UserWalletRepo], WalletError, List[Wallet]] =
    ZIO.serviceWith[UserWalletRepo](_.getWallets(userId))

  def removeWallet(userId: UserId, address: WalletAddress): ZIO[Has[UserWalletRepo], WalletError, Unit] =
    ZIO.serviceWith[UserWalletRepo](_.removeWallet(userId, address))
}
