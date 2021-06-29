package io.softwarechain.cryptojournal
package infrastructure.google

import domain.position.Position.PositionId
import domain.position.error.JournalSaveError
import domain.position.{JournalEntry, JournalingRepo}
import domain.model.UserId
import infrastructure.google.DatastoreJournalingRepo.{KeyDelimiter, Journal}

import com.google.cloud.datastore.{Datastore, Entity}
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer}

final case class DatastoreJournalingRepo(datastore: Datastore, logger: Logger[String]) extends JournalingRepo {
  override def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit] = {
    Task(datastore.put(journalEntryToEntity(userId, positionId, entry)))
      .tapError(throwable => logger.error(s"Unable to save journal entry - ${throwable.getMessage}"))
      .mapError(throwable => JournalSaveError(throwable))
      .unit
  }

  val journalEntryToEntity: (UserId, PositionId, JournalEntry) => Entity = (userId, positionId, entry) => {
    Entity.newBuilder(datastore.newKeyFactory().setKind(Journal).newKey(userId.value + KeyDelimiter + positionId.value))
      .set("notes", entry.notes.map(_.value).getOrElse(""))
      .build()
  }
}

object DatastoreJournalingRepo {
  lazy val layer: URLayer[Has[Datastore] with Logging, Has[JournalingRepo]] = (DatastoreJournalingRepo(_, _)).toLayer

  val Journal = "Journal"

  val KeyDelimiter = "#"
}