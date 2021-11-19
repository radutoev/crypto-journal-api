package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{Currency, FungibleData, WalletAddress}
import domain.wallet.error.WalletError

import zio.IO

//TODO Remove this, it is deprecated
trait WalletRepo {
  def getQuote(address: WalletAddress, coin: Currency): IO[WalletError, FungibleData]
}
