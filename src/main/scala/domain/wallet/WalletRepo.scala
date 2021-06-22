package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import error.WalletError

import zio.{Has, IO, ZIO}

trait WalletRepo {
  def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]
}

object WalletRepo {
  def addWallet(userId: UserId, address: WalletAddress): ZIO[Has[WalletRepo], WalletError, Unit] =
    ZIO.serviceWith[WalletRepo](_.addWallet(userId, address))

  def getWallets(userId: UserId): ZIO[Has[WalletRepo], WalletError, List[Wallet]] =
    ZIO.serviceWith[WalletRepo](_.getWallets(userId))

  def removeWallet(userId: UserId, address: WalletAddress): ZIO[Has[WalletRepo], WalletError, Unit] =
    ZIO.serviceWith[WalletRepo](_.removeWallet(userId, address))
}
