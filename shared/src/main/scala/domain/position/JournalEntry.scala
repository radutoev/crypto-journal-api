package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Mistake, PlayId, Tag, UserId }
import domain.position.model.ScamStrategy

final case class JournalEntry(
  notes: Option[String],
  tags: List[Tag],
  mistakes: List[Mistake],
  scamStrategy: Option[ScamStrategy] = None,
  userId: Option[UserId] = None,
  positionId: Option[PlayId] = None
)
