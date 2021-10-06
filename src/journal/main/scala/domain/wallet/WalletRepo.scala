package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress
import domain.wallet.error.WalletError

import zio.IO

trait WalletRepo {
  def addWallet(address: WalletAddress): IO[WalletError, Unit]

  def exists(address: WalletAddress): IO[WalletError, Boolean]
}
