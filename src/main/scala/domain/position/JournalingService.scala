package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error._
import domain.position.Position.PositionId

import zio.{ Function1ToLayerSyntax, Has, IO, URLayer }

trait JournalingService {
  def saveJournalEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def addSetups(userId: UserId, tagPositions: TagPositions): IO[SetupSaveError, Unit]

  def addMistakes(userId: UserId, tagPositions: TagPositions): IO[MistakeSaveError, Unit]
}

final case class LiveJournalingService(journalingRepo: JournalingRepo) extends JournalingService {
  override def saveJournalEntry(
    userId: UserId,
    positionId: PositionId,
    entry: JournalEntry
  ): IO[JournalSaveError, Unit] =
    journalingRepo.saveEntry(userId, positionId, entry)

  override def addSetups(userId: UserId, tagPositions: TagPositions): IO[SetupSaveError, Unit] =
    journalingRepo.addSetups(userId, tagPositions)

  override def addMistakes(userId: UserId, tagPositions: TagPositions): IO[MistakeSaveError, Unit] =
    journalingRepo.addMistakes(userId, tagPositions)
}

object LiveJournalingService {
  lazy val layer: URLayer[Has[JournalingRepo], Has[JournalingService]] = (LiveJournalingService(_)).toLayer
}
