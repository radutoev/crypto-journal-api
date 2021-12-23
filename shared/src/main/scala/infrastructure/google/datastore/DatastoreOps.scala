package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import com.google.cloud.datastore.{Datastore, Query, QueryResults, ReadOption}
import zio.Task
import zio.logging.Logger

trait DatastoreOps {
  def executeQuery[Result](query: Query[Result])(implicit datastore: Datastore, logger: Logger[String]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))
}
