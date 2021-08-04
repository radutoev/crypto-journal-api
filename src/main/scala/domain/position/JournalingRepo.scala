package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error._
import domain.position.Position.PositionId

import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry]

  def getEntries(userId: UserId, ids: List[PositionId]): IO[PositionError, List[JournalEntry]]

  def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def addSetups(userId: UserId, positionTags: List[PositionTags]): IO[SetupSaveError, Unit]

  def addMistakes(userId: UserId, positionTags: List[PositionTags]): IO[MistakeSaveError, Unit]
}
