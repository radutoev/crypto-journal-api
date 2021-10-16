package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Mistake, Tag, UserId }
import domain.position.Position.PositionId

final case class JournalEntry(
  notes: Option[String],
  tags: List[Tag],
  mistakes: List[Mistake],
  userId: Option[UserId] = None,
  positionId: Option[PositionId] = None
)
