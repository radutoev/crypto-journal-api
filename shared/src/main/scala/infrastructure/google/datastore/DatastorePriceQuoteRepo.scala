package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.{ CovalentConfig, DatastoreConfig }
import domain.model.Currency
import domain.pricequote.error.{ PriceQuoteError, PriceQuoteFetchError }
import domain.pricequote.{ PriceQuote, PriceQuoteRepo }
import util.InstantOps
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore._
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant
import java.util.UUID
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteRepo {

  override def getQuotes(
    currencies: Set[Currency],
    interval: TimeInterval
  ): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]] =
    for {
      _     <- logger.info(s"Fetch quotes in $interval for currencies ${currencies.mkString(",")}")
      start = interval.start.atBeginningOfDay()
      //TODO composition equality filter doesn't seem to work.
      eqFilters = currencies.map(c => PropertyFilter.eq("currency", c.value)).toList
      filter = CompositeFilter.and(
        PropertyFilter.ge("timestamp", Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano))
//        eqFilters: _*
      )
      query = Query
        .newEntityQueryBuilder()
        .setKind(datastoreConfig.priceQuote)
        .setFilter(filter)
        .addOrderBy(OrderBy.asc("timestamp"))
        .build()
      results <- Task(datastore.run(query, Seq.empty[ReadOption]: _*))
                  .tapError(err => logger.warn(err.getMessage))
                  .orElseFail(PriceQuoteFetchError("Unable to fetch quotes"))
      quotes = results.asScala.toList
        .map(entityToPriceQuote)
        .filter(t => t._2.timestamp.isBefore(interval.end) || t._2.timestamp == interval.end)
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._2))
        .toMap
    } yield quotes

  override def getLatestQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Map[Currency, PriceQuote]] =
    if (currencies.nonEmpty) {
      UIO(Map.empty)

//      val filter = currencies.map(currency => PropertyFilter.eq("currency", currency.value))
//
//      //TODO Figure why this doesn't work.
//      val query = Query.newProjectionEntityQueryBuilder()
//        .setKind(datastoreConfig.priceQuote)
//        .setFilter(CompositeFilter.and(filter.head, filter.tail.toList: _*))
//        .setProjection("currency", "timestamp", "price")
//        .setDistinctOn("currency")
//        .setOrderBy(OrderBy.asc("currency"), OrderBy.desc("timestamp"), OrderBy.asc("price"))
//        .build()
//
//      Task(datastore.run(query, Seq.empty[ReadOption]: _*))
//        .map(results => results.asScala.toList.map { entity =>
//          (
//            Currency.unsafeFrom(entity.getString("currency")),
//            PriceQuote(
//              Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
//              Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
//            )
//          )
//        }.toMap)
//        .tapError(err => logger.warn(err.getMessage))
//        .orElseFail(PriceQuoteFetchError("Unable to fetch latest quotes"))
    } else {
      UIO(Map.empty)
    }

  override def saveQuotes(quotes: Map[Currency, List[PriceQuote]]): IO[PriceQuoteError, Unit] = {
    @inline
    def saveEntities(list: List[Entity]) =
      Task(datastore.newTransaction())
        .bracket(txn => Task(txn.rollback()).when(txn.isActive).ignore) { txn =>
          Task {
            txn.put(list: _*)
            txn.commit()
          } *> logger.info(s"Imported ${list.size} quotes")
        }
        .tapError(throwable =>
          logger.error(s"Error saving quotes: ${list.mkString(",")}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle transactions response when doing error handling.

    {
      val entities = quotes.flatMap {
        case (currency, quotes) =>
          quotes.map(quote => priceQuoteWithCurrencyToEntity(currency -> quote))
      }.grouped(25).toList
      for {
        _ <- ZIO.foreach(entities)(items => saveEntities(items.toList)).ignore
        _ <- logger.info(s"Finished upsert of quotes")
      } yield ()
    }.when(quotes.nonEmpty)
  }

  private val entityToPriceQuote: Entity => (Currency, PriceQuote) = entity => {
    (
      Currency.unsafeFrom(entity.getString("currency")),
      PriceQuote(
        Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
        Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
      )
    )
  }

  private def priceQuoteWithCurrencyToEntity(data: (Currency, PriceQuote)) =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.priceQuote).newKey(UUID.randomUUID().toString))
      .set("currency", data._1.value)
      .set(
        "timestamp",
        TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(data._2.timestamp.getEpochSecond, data._2.timestamp.getNano))
      )
      .set("price", data._2.price)
      .build()
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _)).toLayer
}
