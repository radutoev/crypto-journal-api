package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.Position.PositionId

import eu.timepit.refined.types.string.NonEmptyString

final case class JournalEntry(
  notes: Option[String],
  setups: List[NonEmptyString],
  mistakes: List[NonEmptyString],
  userId: Option[UserId] = None,
  positionId: Option[PositionId] = None
)
