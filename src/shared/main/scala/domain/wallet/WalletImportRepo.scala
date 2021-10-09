package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus

import zio.IO

trait WalletImportRepo {
  def addWallet(address: WalletAddress): IO[WalletError, Unit]

  def getByImportStatus(status: WalletImportStatus): IO[WalletError, List[WalletAddress]]

  def exists(address: WalletAddress): IO[WalletError, Boolean]

  def updateImportStatus(address: WalletAddress, state: WalletImportStatus): IO[WalletError, Unit]

  def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus]
}
