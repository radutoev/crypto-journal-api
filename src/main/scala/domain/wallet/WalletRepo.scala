package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import domain.error.WalletError

import zio.{Has, IO, ZIO}

trait WalletRepo {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]
}

object WalletRepo {
  def addWallet(userId: UserId, walletAddress: WalletAddress): ZIO[Has[WalletRepo], WalletError, Unit] =
    ZIO.serviceWith[WalletRepo](_.addWallet(userId, walletAddress))
}
