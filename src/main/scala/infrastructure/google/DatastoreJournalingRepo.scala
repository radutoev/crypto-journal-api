package io.softwarechain.cryptojournal
package infrastructure.google

import domain.position.Position.PositionId
import domain.position.error.{
  InvalidRepresentation,
  JournalFetchError,
  JournalNotFound,
  JournalSaveError,
  PositionError
}
import domain.position.{ JournalEntry, JournalingRepo }
import domain.model.UserId
import infrastructure.google.DatastoreJournalingRepo.{ entityToJournalEntry, journalEntryKey, Journal }
import util.tryOrLeft

import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore.{ Datastore, Entity, Query, QueryResults, ReadOption }
import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer, ZIO }

import scala.jdk.CollectionConverters._

final case class DatastoreJournalingRepo(datastore: Datastore, logger: Logger[String]) extends JournalingRepo {
  override def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry] = {
    val key   = datastore.newKeyFactory().setKind(Journal).newKey(journalEntryKey(userId, positionId))
    val query = Query.newEntityQueryBuilder().setKind(Journal).setFilter(PropertyFilter.eq("__key__", key)).build()
    executeQuery(query)
      .mapError(throwable => JournalFetchError(throwable))
      .flatMap { queryResult =>
        val results = queryResult.asScala
        if (results.nonEmpty) {
          ZIO.fromEither(entityToJournalEntry(results.next()))
        } else {
          ZIO.fail(JournalNotFound(userId, positionId))
        }
      }
  }

  override def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit] =
    Task(datastore.put(journalEntryToEntity(userId, positionId, entry)))
      .tapError(throwable => logger.error(s"Unable to save journal entry - ${throwable.getMessage}"))
      .mapError(throwable => JournalSaveError(throwable))
      .unit

  val journalEntryToEntity: (UserId, PositionId, JournalEntry) => Entity = (userId, positionId, entry) => {
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(Journal).newKey(journalEntryKey(userId, positionId)))
      .set("notes", entry.notes.map(_.value).getOrElse(""))
      .build()
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
}

object DatastoreJournalingRepo {
  lazy val layer: URLayer[Has[Datastore] with Logging, Has[JournalingRepo]] = (DatastoreJournalingRepo(_, _)).toLayer

  val Journal = "Journal"

  val KeyDelimiter = "#"

  val journalEntryKey: (UserId, PositionId) => String = (uId, pId) => uId.value + KeyDelimiter + pId.value

  val entityToJournalEntry: Entity => Either[InvalidRepresentation, JournalEntry] = entity => {
    for {
      notes <- tryOrLeft(entity.getString("notes"), InvalidRepresentation("Entry has no key notes"))
                .map(rawNotesStr => refined.refineV[NonEmpty](rawNotesStr).map(Some(_)).getOrElse(None))
    } yield JournalEntry(notes)
  }
}
