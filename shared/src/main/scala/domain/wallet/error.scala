package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}

object error {
  sealed trait WalletError
  final case class InvalidWallet(reason: String)                                  extends WalletError
  final case class WalletAddressExists(address: WalletAddress)                    extends WalletError
  final case class UnableToAddWallet(address: WalletAddress)                      extends WalletError
  final case class UnableToRemoveWallet(address: WalletAddress)                   extends WalletError
  final case class WalletNotFound(userId: UserId, address: WalletAddress)         extends WalletError
  final case class WalletFetchError(address: WalletAddress, throwable: Throwable) extends WalletError
  final case class WalletsFetchError(throwable: Throwable)                        extends WalletError
  final case class WalletMessagingError(throwable: Throwable)                     extends WalletError
}
