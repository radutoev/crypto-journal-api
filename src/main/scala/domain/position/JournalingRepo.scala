package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error._
import domain.position.Position.PositionId
import domain.position.TagPositions

import zio.IO

trait JournalingRepo {
  def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry]

  def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def addSetups(userId: UserId, tagPositions: TagPositions): IO[SetupSaveError, Unit]

  def addMistakes(userId: UserId, tagPositions: TagPositions): IO[MistakeSaveError, Unit]
}
