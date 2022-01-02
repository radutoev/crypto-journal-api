package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import com.google.cloud.datastore.{Datastore, Entity, Key, Query, QueryResults, ReadOption}
import zio.Task
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

  def get(keys: Set[Key])(implicit datastore: Datastore, logger: Logger[String]): Task[List[Entity]] =
    Task(datastore.get(keys.asJava))
      .tapError(throwable => logger.warn(throwable.getMessage))
      .map(_.asScala.toList)
}
