package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.currency.CurrencyRepo
import domain.currency.error.{CurrencyError, CurrencyFetchError}
import domain.model.Currency

import com.google.cloud.datastore.{Datastore, Entity, Query, ReadOption}
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer}

import scala.jdk.CollectionConverters._

final case class DatastoreCurrencyRepo (datastore: Datastore,
                                        datastoreConfig: DatastoreConfig,
                                        logger: Logger[String]) extends CurrencyRepo {
  override def getCurrencies(): IO[CurrencyError, Set[Currency]] = {
    for {
      _     <- logger.info("Fetch currencies")
      query = Query.newEntityQueryBuilder().setKind(datastoreConfig.currency).build()
      results <- Task(datastore.run(query, Seq.empty[ReadOption]:_*)).tapError(err => logger.warn(err.getMessage)).orElseFail(CurrencyFetchError())
    } yield results.asScala.toSet.map(entityToCurrency)
  }

  private def entityToCurrency(entity: Entity): Currency = {
    Currency.unsafeFrom(entity.getString("currency"))
  }
}

object DatastoreCurrencyRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging, Has[DatastoreCurrencyRepo]] =
    (DatastorePriceQuoteRepo(_, _, _)).toLayer
}
