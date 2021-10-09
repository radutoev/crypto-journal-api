package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportState

import zio.IO

trait WalletImportRepo {
  def addWallet(address: WalletAddress): IO[WalletError, Unit]

  def exists(address: WalletAddress): IO[WalletError, Boolean]

  def updateImportStatus(address: WalletAddress, state: WalletImportState): IO[WalletError, Unit]

  def getImportState(address: WalletAddress): IO[WalletError, WalletImportState]
}
