package io.softwarechain.cryptojournal
package domain.wallet

object event {
  sealed trait WalletEvent
  final case class WalletAdded(wallet: Wallet)   extends WalletEvent
  final case class WalletRemoved(wallet: Wallet) extends WalletEvent
}
