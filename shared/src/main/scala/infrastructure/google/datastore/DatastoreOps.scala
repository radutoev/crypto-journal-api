package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import com.google.cloud.datastore.{Datastore, Entity, Key, Query, QueryResults, ReadOption}
import zio.{IO, Task, ZIO}
import zio.logging.Logger

import scala.jdk.CollectionConverters.{IterableHasAsJava, IteratorHasAsScala}

trait DatastoreOps {
  def executeQuery[Result](query: Query[Result])(implicit datastore: Datastore, logger: Logger[String]): Task[QueryResults[Result]] =
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
    def doGet(batch: Set[Key]): IO[Option[String], List[Entity]] = {
      Task(datastore.get(batch.asJava))
        .tapError(throwable => logger.warn(throwable.getMessage))
        .mapBoth(_ => Some(s"Unable to fetch data for $batch"), _.asScala.toList)
    }

    ZIO.collect(keys.grouped(1000).toList)(doGet)
      .mapBoth(
        _ => new RuntimeException(s"Unable to fetch data for $keys"),
        _.flatten.toList
      )
  }
}
