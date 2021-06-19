package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{ UserId, WalletAddress }

object error {
  sealed trait CryptoError

  sealed trait WalletError                                                                        extends CryptoError
  final case class InvalidWallet(reason: String)                                                  extends WalletError
  final case class WalletAddressExists(address: WalletAddress)                                    extends WalletError
  final case class UnableToAddWallet(address: WalletAddress)                                      extends WalletError
  final case class UnableToRemoveWallet(address: WalletAddress)                                   extends WalletError
  final case class WalletNotFound(userId: UserId, address: WalletAddress)                         extends WalletError
  final case class WalletFetchError(userId: UserId, address: WalletAddress, throwable: Throwable) extends WalletError
  final case class WalletsFetchError(userId: UserId, throwable: Throwable)                        extends WalletError
}
