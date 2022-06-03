package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import com.google.cloud.datastore.{Cursor, Datastore, Entity, Key, KeyQuery, Query, QueryResults, ReadOption}
import com.google.datastore.v1.QueryResultBatch.MoreResultsType
import eu.timepit.refined.types.string.NonEmptyString
import zio.{IO, Task, ZIO}
import zio.logging.Logger

import scala.jdk.CollectionConverters.{IterableHasAsJava, IteratorHasAsScala}

trait DatastoreOps {
  def executeQuery[Result](
    query: Query[Result]
  )(implicit datastore: Datastore, logger: Logger[String]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  def get(key: Key)(implicit datastore: Datastore, logger: Logger[String]): Task[Option[Entity]] =
    Task(datastore.get(key, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
      .map(Option(_))

  /**
   * Datastore has a limit of 1000 keys that can be fetched.
   *  -> "cannot get more than 1000 keys in a single call" exception.
   */
  def get(keys: Set[Key])(implicit datastore: Datastore, logger: Logger[String]): Task[List[Entity]] = {
    def doGet(batch: Set[Key]): IO[Option[String], List[Entity]] =
      Task(datastore.get(batch.asJava))
        .tapError(throwable => logger.warn(throwable.getMessage))
        .mapBoth(_ => Some(s"Unable to fetch data for $batch"), _.asScala.toList)

    ZIO
      .collect(keys.grouped(1000).toList)(doGet)
      .mapBoth(
        _ => new RuntimeException(s"Unable to fetch data for $keys"),
        _.flatten.toList
      )
  }

  def deleteAll(kind: NonEmptyString)(implicit datastore: Datastore, logger: Logger[String]): Task[Unit] = {
    deleteAll(Query.newKeyQueryBuilder().setKind(kind.value))
  }

  def deleteAll(qb: KeyQuery.Builder)(implicit datastore: Datastore, logger: Logger[String]): Task[Unit] = {
    val limitQb = qb.setLimit(1000)

    def deleteByKeys(result: QueryResults[Key]): Task[Unit] = {
      val moreResultsType = result.getMoreResults
      val moreDataToFetch =
        moreResultsType == MoreResultsType.MORE_RESULTS_AFTER_LIMIT || moreResultsType == MoreResultsType.NOT_FINISHED
      if (moreDataToFetch) {
        val cursor = result.getCursorAfter
        val keys   = result.asScala.toList
        Task(datastore.delete(keys: _*))
          .tapError(t => logger.warn(t.getMessage))
          .flatMap(_ => deleteFromCursor(cursor))
      } else {
        ZIO.unit
      }
    }

    def deleteFromCursor(cursor: Cursor): Task[Unit] =
      executeQuery(limitQb.setStartCursor(cursor).build())(datastore, logger)
        .flatMap(deleteByKeys)

    executeQuery(limitQb.build())(datastore, logger)
      .flatMap(deleteByKeys)
  }
}
