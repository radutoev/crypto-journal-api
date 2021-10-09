package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus

import zio.{Has, IO, ZIO}

trait WalletImportRepo {
  def addWallet(address: WalletAddress): IO[WalletError, Unit]

  def getByImportStatus(status: WalletImportStatus): IO[WalletError, List[WalletAddress]]

  def exists(address: WalletAddress): IO[WalletError, Boolean]

  def updateImportStatus(address: WalletAddress, state: WalletImportStatus): IO[WalletError, Unit]

  def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus]
}

object WalletImportRepo {
  def getByImportStatus(status: WalletImportStatus): ZIO[Has[WalletImportRepo], WalletError, List[WalletAddress]] = {
    ZIO.serviceWith[WalletImportRepo](_.getByImportStatus(status))
  }
}
