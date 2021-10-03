package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Mistake, Setup, UserId}
import domain.position.Position.PositionId

final case class JournalEntry(
  notes: Option[String],
  setups: List[Setup],
  mistakes: List[Mistake],
  userId: Option[UserId] = None,
  positionId: Option[PositionId] = None
)
