package io.softwarechain.cryptojournal
package domain

import domain.wallet.event.WalletEvent

import zio.stream.ZStream

trait WalletStream {
  def walletStream(): ZStream[Any, Nothing, WalletEvent]
}
