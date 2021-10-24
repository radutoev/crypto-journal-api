package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Mistake, PlayId, Tag, UserId }

final case class JournalEntry(
  notes: Option[String],
  tags: List[Tag],
  mistakes: List[Mistake],
  userId: Option[UserId] = None,
  positionId: Option[PlayId] = None
)
