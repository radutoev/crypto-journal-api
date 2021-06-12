package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Fee, State, TransactionType }

import java.time.Instant

final case class Position(
  coin: String,
  state: State,
  openedAt: Instant,
  closedAt: Option[Instant],
  entries: List[PositionEntry] = List.empty
)

final case class PositionEntry(`type`: TransactionType, fee: Fee, timestamp: Instant)
