package io.softwarechain.cryptojournal
package infrastructure.journal

import domain.wallet.Wallet

object dto {
  /* @deprecated */
  sealed trait WalletUpdate
  final case class WalletAdded(wallet: Wallet)   extends WalletUpdate
  final case class WalletRemoved(wallet: Wallet) extends WalletUpdate
}
