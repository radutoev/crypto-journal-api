package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus

import zio.{ Has, IO, ZIO }

trait WalletRepo {
  def addWallet(address: WalletAddress): IO[WalletError, Unit]

  def getByImportStatus(status: WalletImportStatus): IO[WalletError, List[WalletAddress]]

  def exists(address: WalletAddress): IO[WalletError, Boolean]

  def updateImportStatus(address: WalletAddress, state: WalletImportStatus): IO[WalletError, Unit]

  def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus]

  def getWallets(): IO[WalletError, List[WalletAddress]]
}

object WalletRepo {
  def getByImportStatus(status: WalletImportStatus): ZIO[Has[WalletRepo], WalletError, List[WalletAddress]] =
    ZIO.serviceWith[WalletRepo](_.getByImportStatus(status))
}
