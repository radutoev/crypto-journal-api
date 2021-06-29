package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error.{PositionError, JournalSaveError}
import domain.position.Position.PositionId

import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry]

  def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]
}
