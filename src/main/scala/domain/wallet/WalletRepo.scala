package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import domain.error.WalletError

import zio.{Has, IO, ZIO}

trait WalletRepo {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]
}

object WalletRepo {
  def addWallet(userId: UserId, walletAddress: WalletAddress): ZIO[Has[WalletRepo], WalletError, Unit] =
    ZIO.serviceWith[WalletRepo](_.addWallet(userId, walletAddress))

  def getWallets(userId: UserId): ZIO[Has[WalletRepo], WalletError, List[Wallet]] =
    ZIO.serviceWith[WalletRepo](_.getWallets(userId))
}
