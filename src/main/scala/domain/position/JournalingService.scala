package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error._
import domain.position.Position.PositionId

import zio.{ Function1ToLayerSyntax, Has, IO, URLayer }

trait JournalingService {
  def saveJournalEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def addSetups(userId: UserId, positionTags: List[PositionTags]): IO[SetupSaveError, Unit]

  def addMistakes(userId: UserId, positionTags: List[PositionTags]): IO[MistakeSaveError, Unit]
}

final case class LiveJournalingService(journalingRepo: JournalingRepo) extends JournalingService {
  override def saveJournalEntry(
    userId: UserId,
    positionId: PositionId,
    entry: JournalEntry
  ): IO[JournalSaveError, Unit] =
    journalingRepo.saveEntry(userId, positionId, entry)

  override def addSetups(userId: UserId, positionTags: List[PositionTags]): IO[SetupSaveError, Unit] =
    journalingRepo.addSetups(userId, positionTags)

  override def addMistakes(userId: UserId, positionTags: List[PositionTags]): IO[MistakeSaveError, Unit] =
    journalingRepo.addMistakes(userId, positionTags)
}

object LiveJournalingService {
  lazy val layer: URLayer[Has[JournalingRepo], Has[JournalingService]] = (LiveJournalingService(_)).toLayer
}
