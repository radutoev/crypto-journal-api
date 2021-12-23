package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{ UserId, WalletAddress }
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus

import zio.IO

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]

  def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus]
}
