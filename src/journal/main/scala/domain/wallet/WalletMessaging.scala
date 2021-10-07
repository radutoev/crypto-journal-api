package io.softwarechain.cryptojournal
package domain.wallet

import domain.wallet.error.WalletError
import domain.wallet.event.WalletEvent

import zio.IO

trait WalletMessaging {
  def publish(event: WalletEvent): IO[WalletError, Unit]
}
