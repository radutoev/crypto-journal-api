package io.softwarechain.cryptojournal
package domain.position

import domain.position.error._

import io.softwarechain.cryptojournal.domain.model.{PlayId, UserId}
import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, playId: PlayId): IO[MarketPlayError, JournalEntry]

  def getEntries(userId: UserId, ids: List[PlayId]): IO[MarketPlayError, List[JournalEntry]]

  def saveEntry(userId: UserId, playId: PlayId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def saveEntries(userId: UserId, positionEntries: List[PositionJournalEntry]): IO[JournalSaveError, Unit]
}
