package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error._
import domain.position.Position.PositionId

import zio.{ Function1ToLayerSyntax, Has, IO, URLayer }

trait JournalingService {
  def saveJournalEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]
}

final case class LiveJournalingService(journalingRepo: JournalingRepo) extends JournalingService {
  override def saveJournalEntry(
    userId: UserId,
    positionId: PositionId,
    entry: JournalEntry
  ): IO[JournalSaveError, Unit] =
    journalingRepo.saveEntry(userId, positionId, entry)
}

object LiveJournalingService {
  lazy val layer: URLayer[Has[JournalingRepo], Has[JournalingService]] = (LiveJournalingService(_)).toLayer
}
