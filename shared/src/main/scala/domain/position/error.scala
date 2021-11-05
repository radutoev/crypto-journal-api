package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ContextId, PlayId, UserId, WalletAddress}
import vo.pagination.PaginationContext

object error {
  sealed trait MarketPlayError
  final case class InvalidRepresentation(message: String) extends MarketPlayError

  final case class MarketPlaysFetchError(address: WalletAddress) extends MarketPlayError

  final case class MarketPlayNotFound(playId: PlayId)                         extends MarketPlayError
  final case class MarketPlayFetchError(playId: PlayId, throwable: Throwable) extends MarketPlayError

  final case class PaginationContextSaveError(context: PaginationContext) extends MarketPlayError
  final case class PaginationContextNotFoundError(contextId: ContextId)   extends MarketPlayError
  final case class PaginationContextFetchError(contextId: ContextId)      extends MarketPlayError

  final case class PriceQuotesError(throwable: Throwable) extends MarketPlayError

  final case class CheckpointFetchError(address: WalletAddress, throwable: Throwable) extends MarketPlayError
  final case class CheckpointNotFound(address: WalletAddress)                         extends MarketPlayError

  final case class JournalSaveError(throwable: Throwable)              extends MarketPlayError
  final case class JournalFetchError(throwable: Throwable)             extends MarketPlayError
  final case class JournalNotFound(userId: UserId, positionId: PlayId) extends MarketPlayError

  final case class InvalidInput(reason: String) extends MarketPlayError

  final case class MarketPlayImportError(address: WalletAddress, throwable: Throwable) extends MarketPlayError
}
