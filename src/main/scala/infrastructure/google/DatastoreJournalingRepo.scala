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
import infrastructure.google.DatastoreJournalingRepo.{ entityToJournalEntry, journalEntryKey }
import util.tryOrLeft

import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore.{ Datastore, Entity, Query, QueryResults, ReadOption, StringValue }
import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.string.NonEmptyString
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer, ZIO }

import scala.jdk.CollectionConverters._

final case class DatastoreJournalingRepo(datastore: Datastore, datastoreConfig: DatastoreConfig, logger: Logger[String])
    extends JournalingRepo {

  override def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.journalKind).newKey(journalEntryKey(userId, positionId))
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.journalKind)
      .setFilter(PropertyFilter.eq("__key__", key))
      .build()
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
      .newBuilder(
        datastore.newKeyFactory().setKind(datastoreConfig.journalKind).newKey(journalEntryKey(userId, positionId))
      )
      .set("notes", entry.notes.map(_.value).getOrElse(""))
      .set("setups", entry.setups.map(_.value).map(StringValue.of).asJava)
      .set("mistakes", entry.mistakes.map(_.value).map(StringValue.of).asJava)
      .build()
  }

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
}

object DatastoreJournalingRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging, Has[JournalingRepo]] =
    (DatastoreJournalingRepo(_, _, _)).toLayer

  val KeyDelimiter = "#"

  val journalEntryKey: (UserId, PositionId) => String = (uId, pId) => uId.value + KeyDelimiter + pId.value

  val entityToJournalEntry: Entity => Either[InvalidRepresentation, JournalEntry] = entity => {
    for {
      notes <- tryOrLeft(entity.getString("notes"), InvalidRepresentation("Entry has no key notes"))
                .map(rawNotesStr => refined.refineV[NonEmpty](rawNotesStr).map(Some(_)).getOrElse(None))
      setups <- tryOrLeft(
                 if (entity.contains("setups")) entity.getList[StringValue]("setups") else List.empty.asJava,
                 InvalidRepresentation("Invalid setups representation")
               ).map(rawSetups => rawSetups.asScala.map(strValue => NonEmptyString.unsafeFrom(strValue.get())).toList)
      mistakes <- tryOrLeft(
                   if (entity.contains("mistakes")) entity.getList[StringValue]("mistakes") else List.empty.asJava,
                   InvalidRepresentation("Invalid mistakes representation")
                 ).map(rawSetups => rawSetups.asScala.map(strValue => NonEmptyString.unsafeFrom(strValue.get())).toList)
    } yield JournalEntry(notes, setups, mistakes)
  }
}
