package io.softwarechain.cryptojournal
package infrastructure.google

import config.DatastoreConfig
import domain.model.{ MistakePredicate, SetupPredicate, UserId, UserIdPredicate }
import domain.position.Position.{ PositionId, PositionIdPredicate }
import domain.position.error._
import domain.position.{ JournalEntry, JournalingRepo, PositionJournalEntry }
import infrastructure.google.DatastoreJournalingRepo.{ entityToJournalEntry, journalEntryKey }
import util.tryOrLeft

import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore._
import eu.timepit.refined
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer, ZIO }

import scala.jdk.CollectionConverters._

final case class DatastoreJournalingRepo(datastore: Datastore, datastoreConfig: DatastoreConfig, logger: Logger[String])
    extends JournalingRepo {

  override def getEntry(userId: UserId, positionId: PositionId): IO[PositionError, JournalEntry] = {
    val key = datastore.newKeyFactory().setKind(datastoreConfig.journal).newKey(journalEntryKey(userId, positionId))
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.journal)
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

  override def getEntries(userId: UserId, ids: List[PositionId]): IO[PositionError, List[JournalEntry]] = {
    val kind = datastore.newKeyFactory().setKind(datastoreConfig.journal)
    val keys = ids.map(id => kind.newKey(journalEntryKey(userId, id)))
    Task(datastore.get(keys: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
      .bimap(
        JournalFetchError,
        results =>
          results.asScala.toList.map(entityToJournalEntry).collect {
            case Right(journalEntry) => journalEntry
          }
      )
  }

  override def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit] =
    Task(datastore.put(journalEntryToEntity(userId, positionId, entry)))
      .tapError(throwable => logger.error(s"Unable to save journal entry - ${throwable.getMessage}"))
      .mapError(throwable => JournalSaveError(throwable))
      .unit

  override def saveEntries(userId: UserId, positionEntries: List[PositionJournalEntry]): IO[JournalSaveError, Unit] = {
    val batches = positionEntries
      .map(posEntry => journalEntryToEntity(userId, posEntry.positionId, posEntry.entry))
      .grouped(25)
      .toList
    logger.info(s"Saving ${batches.size} batches") *>
    ZIO
      .foreach(batches) { entities =>
        Task(datastore.put(entities: _*))
          .tapError(err => logger.warn(s"Failure saving entities: ${err.getMessage}"))
      }
      .mapError(t => JournalSaveError(t))
      .unit
  }

  val journalEntryToEntity: (UserId, PositionId, JournalEntry) => Entity = (userId, positionId, entry) => {
    Entity
      .newBuilder(
        datastore.newKeyFactory().setKind(datastoreConfig.journal).newKey(journalEntryKey(userId, positionId))
      )
      .set("notes", entry.notes.getOrElse(""))
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
      key    <- tryOrLeft(entity.getKey().getName, InvalidRepresentation("Entry has no key name"))
      parts  = key.split(KeyDelimiter)
      userId <- refined.refineV[UserIdPredicate](parts.head).left.map(_ => InvalidRepresentation("Invalid user id"))
      positionId <- refined
                     .refineV[PositionIdPredicate](parts.last)
                     .left
                     .map(_ => InvalidRepresentation("Invalid position id"))
      notes <- tryOrLeft(
                if (entity.contains("notes")) entity.getString("notes") else "",
                InvalidRepresentation("Invalid notes representation")
              ).map(rawNotesStr => if (rawNotesStr.nonEmpty) Some(rawNotesStr) else None)
      setups <- tryOrLeft(
                 if (entity.contains("setups")) entity.getList[StringValue]("setups") else List.empty.asJava,
                 InvalidRepresentation("Invalid setups representation")
               ).map(rawSetups =>
                 rawSetups.asScala.map(strValue => refined.refineV[SetupPredicate].unsafeFrom(strValue.get())).toList
               )
      mistakes <- tryOrLeft(
                   if (entity.contains("mistakes")) entity.getList[StringValue]("mistakes") else List.empty.asJava,
                   InvalidRepresentation("Invalid mistakes representation")
                 ).map(rawSetups =>
                   rawSetups.asScala
                     .map(strValue => refined.refineV[MistakePredicate].unsafeFrom(strValue.get()))
                     .toList
                 )
    } yield JournalEntry(notes, setups, mistakes, Some(userId), Some(positionId))
  }
}
