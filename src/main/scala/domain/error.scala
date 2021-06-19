package io.softwarechain.cryptojournal
package domain

import domain.model.{UserId, WalletAddress}

object error {
  sealed trait CryptoError

  sealed trait WalletError extends CryptoError
  final case class InvalidWallet(reason: String) extends WalletError
  final case class WalletAddressExists(address: WalletAddress) extends WalletError
  final case class UnableToAddWallet(address: WalletAddress) extends WalletError
  final case class WalletNotFound(userId: UserId, address: WalletAddress) extends WalletError
  final case class WalletFetchError(userId: UserId, address: WalletAddress, throwable: Throwable) extends WalletError
}
