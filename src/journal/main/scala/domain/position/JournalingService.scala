package io.softwarechain.cryptojournal
package domain.position

import domain.model.{PlayId, UserId}
import domain.position.error._

import zio.{Function1ToLayerSyntax, Has, IO, URLayer}

trait JournalingService {
  def saveJournalEntry(userId: UserId, positionId: PlayId, entry: JournalEntry): IO[JournalSaveError, Unit]

  def saveJournalEntries(userId: UserId, positionEntries: List[PositionJournalEntry]): IO[JournalSaveError, Unit]
}

final case class LiveJournalingService(journalingRepo: JournalingRepo) extends JournalingService {
  override def saveJournalEntry(
                                 userId: UserId,
                                 positionId: PlayId,
                                 entry: JournalEntry
  ): IO[JournalSaveError, Unit] =
    journalingRepo.saveEntry(userId, positionId, entry)

  override def saveJournalEntries(
    userId: UserId,
    positionEntries: List[PositionJournalEntry]
  ): IO[JournalSaveError, Unit] =
    journalingRepo.saveEntries(userId, positionEntries)
}

object LiveJournalingService {
  lazy val layer: URLayer[Has[JournalingRepo], Has[JournalingService]] = (LiveJournalingService(_)).toLayer
}
