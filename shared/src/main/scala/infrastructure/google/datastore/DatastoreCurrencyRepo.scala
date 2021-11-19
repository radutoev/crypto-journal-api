package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.currency.CurrencyRepo
import domain.currency.error.{CurrencyError, CurrencyFetchError}
import domain.model.Currency

import com.google.cloud.datastore.{Datastore, Entity, Query, ReadOption}
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

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

  override def upsert(currencies: Set[Currency]): IO[CurrencyError, Unit] = {
    @inline
    def saveEntities(list: List[Entity]) =
      Task(datastore.newTransaction())
        .bracket(txn => Task(txn.rollback()).when(txn.isActive).ignore) { txn =>
          Task {
            txn.put(list: _*)
            txn.commit()
          } *> logger.info(s"Imported ${list.size} currencies")
        }
        .tapError(throwable =>
          logger.error(s"Error saving currencies: ${list.mkString(",")}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle transactions response when doing error handling.

    if(currencies.isEmpty) {
      logger.info("No currencies provided for upsert")
    } else {
      val entities = currencies.map(currencyToEntity).grouped(23).toList
      for {
        _ <- ZIO.foreach(entities)(items => saveEntities(items.toList)).ignore
        _ <- logger.info(s"Finished upsert of currencies")
      } yield ()
    }
  }

  private def entityToCurrency(entity: Entity): Currency = {
    Currency.unsafeFrom(entity.getString("currency"))
  }

  private def currencyToEntity(currency: Currency): Entity = {
    Entity.newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.currency).newKey(currency.value))
      .set("currency", currency.value)
      .build()
  }
}

object DatastoreCurrencyRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging, Has[CurrencyRepo]] =
    (DatastoreCurrencyRepo(_, _, _)).toLayer
}
