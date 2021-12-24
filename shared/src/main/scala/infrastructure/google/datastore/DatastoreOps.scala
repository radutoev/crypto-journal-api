package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import com.google.cloud.datastore.{Datastore, Key, Query, QueryResults, ReadOption, Entity}
import zio.Task
import zio.logging.Logger

trait DatastoreOps {
  def executeQuery[Result](query: Query[Result])(implicit datastore: Datastore, logger: Logger[String]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  def get(key: Key)(implicit datastore: Datastore, logger: Logger[String]): Task[Option[Entity]] =
    Task(datastore.get(key, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
      .map(Option(_))
}
