package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Fee, FungibleData, State, TransactionType }

import java.time.Instant

final case class Position(
  coin: String,
  state: State,
  openedAt: Instant,
  closedAt: Option[Instant],
  entries: List[PositionEntry]
)

final case class PositionEntry(`type`: TransactionType, value: FungibleData, fee: Fee, timestamp: Instant)
