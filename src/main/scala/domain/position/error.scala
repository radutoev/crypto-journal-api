package io.softwarechain.cryptojournal
package domain.position

import domain.model.WalletAddress

object error {
  sealed trait PositionError
  final case class InvalidRepresentation(message: String) extends PositionError

  final case class CheckpointFetchError(address: WalletAddress, throwable: Throwable) extends PositionError
  final case class CheckpointNotFound(address: WalletAddress) extends PositionError
}
