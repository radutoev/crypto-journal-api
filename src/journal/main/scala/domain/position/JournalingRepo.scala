package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.Position.PositionId
import domain.position.error._

import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry]

  def getEntries(userId: UserId, ids: List[PositionId]): IO[PositionError, List[JournalEntry]]

  def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def saveEntries(userId: UserId, positionEntries: List[PositionJournalEntry]): IO[JournalSaveError, Unit]
}
