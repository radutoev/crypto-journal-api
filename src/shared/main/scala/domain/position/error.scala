package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ContextId, UserId, WalletAddress}
import domain.position.Position.PositionId
import vo.pagination.PaginationContext

object error {
  sealed trait PositionError
  final case class InvalidRepresentation(message: String) extends PositionError

  final case class PositionsFetchError(address: WalletAddress) extends PositionError

  final case class PositionNotFound(positionId: PositionId)                         extends PositionError
  final case class PositionFetchError(positionId: PositionId, throwable: Throwable) extends PositionError

  final case class PaginationContextSaveError(context: PaginationContext) extends PositionError
  final case class PaginationContextNotFoundError(contextId: ContextId) extends PositionError
  final case class PaginationContextFetchError(contextId: ContextId) extends PositionError

  final case class PriceQuotesError(throwable: Throwable) extends PositionError

  final case class CheckpointFetchError(address: WalletAddress, throwable: Throwable) extends PositionError
  final case class CheckpointNotFound(address: WalletAddress)                         extends PositionError

  final case class JournalSaveError(throwable: Throwable)                  extends PositionError
  final case class JournalFetchError(throwable: Throwable)                 extends PositionError
  final case class JournalNotFound(userId: UserId, positionId: PositionId) extends PositionError

  final case class InvalidInput(reason: String) extends PositionError

  final case class PositionImportError(address: WalletAddress, throwable: Throwable) extends PositionError
}
