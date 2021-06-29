package io.softwarechain.cryptojournal
package domain.position

import domain.position.Position.PositionId
import domain.model.{UserId, WalletAddress}

object error {
  sealed trait PositionError
  final case class InvalidRepresentation(message: String) extends PositionError

  final case class PositionsFetchError(address: WalletAddress) extends PositionError

  final case class PositionNotFound(positionId: PositionId) extends PositionError
  final case class PositionFetchError(positionId: PositionId, throwable: Throwable) extends PositionError

  final case class CheckpointFetchError(address: WalletAddress, throwable: Throwable) extends PositionError
  final case class CheckpointNotFound(address: WalletAddress) extends PositionError

  final case class JournalSaveError(throwable: Throwable) extends PositionError
  final case class JournalFetchError(throwable: Throwable) extends PositionError
  final case class JournalNotFound(userId: UserId, positionId: PositionId) extends PositionError
}
