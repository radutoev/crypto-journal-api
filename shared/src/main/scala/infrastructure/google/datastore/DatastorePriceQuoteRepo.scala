package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.Currency
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError}
import domain.pricequote.{PriceQuote, PriceQuoteRepo}
import util.InstantOps
import vo.{PriceQuotesChunk, TimeInterval}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore._
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

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
        .filter(t => t._3.timestamp.isBefore(interval.end) || t._3.timestamp == interval.end)
        .groupBy(_._1)
        .view
        .mapValues(_.map(_._3))
        .toMap
    } yield quotes

  /**
   * @return Latest quotes for all currencies that have quotes in the system.
   */
  override def getLatestQuotes(): IO[PriceQuoteError, Map[Currency, PriceQuote]] = {
    val query = Query.newProjectionEntityQueryBuilder()
      .setKind(datastoreConfig.priceQuote)
      .setProjection("currency", "timestamp", "price")
      .setDistinctOn("currency")
      .setOrderBy(OrderBy.asc("currency"), OrderBy.desc("timestamp"), OrderBy.asc("price"))
      .build()

    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .map(results => results.asScala.toList.map { entity =>
        (
          Currency.unsafeFrom(entity.getString("currency")),
          PriceQuote(
            Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
            Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
          )
        )
      }.toMap)
      .tapError(err => logger.warn(err.getMessage))
      .orElseFail(PriceQuoteFetchError("Unable to fetch latest quotes"))
  }

  override def saveQuotes(quotesChunk: PriceQuotesChunk): IO[PriceQuoteError, Unit] = {
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
      val entities = quotesChunk.quotes
        .map(quote => priceQuoteWithCurrencyToEntity((quotesChunk.baseCurrency, quotesChunk.quoteCurrency, quote)))
        .grouped(25)
        .toList

      for {
        _ <- ZIO.foreach_(entities)(items => saveEntities(items.toList))
        _ <- logger.info(s"Finished upsert of quotes")
      } yield ()
    }.when(quotesChunk.quotes.nonEmpty)
  }

  private val entityToPriceQuote: Entity => (Currency, Currency, PriceQuote) = entity => {
    (
      Currency.unsafeFrom(entity.getString("baseCurrency")),
      Currency.unsafeFrom(entity.getString("quoteCurrency")),
      PriceQuote(
        Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble),
        Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
      )
    )
  }

  private def priceQuoteWithCurrencyToEntity(data: (Currency, Currency, PriceQuote)) =
    Entity
      .newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.priceQuote).newKey(UUID.randomUUID().toString))
      .set("baseCurrency", data._1.value)
      .set("quoteCurrency", data._2.value)
      .set(
        "timestamp",
        TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(data._3.timestamp.getEpochSecond, data._3.timestamp.getNano))
      )
      .set("price", data._3.price)
      .build()
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _)).toLayer
}
