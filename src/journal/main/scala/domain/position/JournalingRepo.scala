package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ PlayId, UserId }
import domain.position.error._

import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, positionId: PlayId): IO[MarketPlayError, JournalEntry]

  def getEntries(userId: UserId, ids: List[PlayId]): IO[MarketPlayError, List[JournalEntry]]

  def saveEntry(userId: UserId, positionId: PlayId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def saveEntries(userId: UserId, positionEntries: List[PositionJournalEntry]): IO[JournalSaveError, Unit]
}
