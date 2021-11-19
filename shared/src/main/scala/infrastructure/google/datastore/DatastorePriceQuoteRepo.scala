package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.{CovalentConfig, DatastoreConfig}
import domain.model.Currency
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError}
import domain.pricequote.{PriceQuote, PriceQuoteRepo}
import util.InstantOps
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore._
import sttp.client3.httpclient.zio.SttpClient
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, UIO, URLayer}

import java.time.Instant
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteRepo {
  //TODO Revisit the implementation
  override def getQuotes(interval: TimeInterval): Task[List[PriceQuote]] =
    for {
      results <- Task {
                  val start = interval.start.atBeginningOfDay()
                  val filter =
                    PropertyFilter.ge("timestamp", Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano))
                  val query: Query[Entity] = Query
                    .newEntityQueryBuilder()
                    .setKind(datastoreConfig.priceQuote)
                    .setFilter(filter)
                    .addOrderBy(OrderBy.desc("timestamp"))
                    .build()
                  val queryResults: QueryResults[Entity] = datastore.run(query, Seq.empty[ReadOption]: _*)
                  queryResults
                }.tapError(errr => UIO(println(errr)))
//      priceQuotes = results.asScala.toList.map(entityToPriceQuote).sortBy(_.timestamp)(Ordering[Instant])
    } yield List.empty

  override def getQuotes(currencies: Set[Currency], interval: TimeInterval): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]] = {
    for {
      _     <- logger.info(s"Fetch quotes in time interval $interval for currencies ${currencies.mkString(",")}")
      start = interval.start.atBeginningOfDay()
      eqFilters = currencies.map(c => PropertyFilter.eq("currency", StringValue.of(c.value))).toList
      filter = CompositeFilter.and(
        PropertyFilter.ge("timestamp", Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano)),
        eqFilters: _*
      )
      query = Query.newEntityQueryBuilder().setKind(datastoreConfig.priceQuote).setFilter(filter).addOrderBy(OrderBy.desc("timestamp")).build()
      results <- Task(datastore.run(query, Seq.empty[ReadOption]: _*))
        .tapError(err => logger.warn(err.getMessage))
        .orElseFail(PriceQuoteFetchError("Unable to fetch quotes"))
      quotes = results.asScala.toList.map(entityToPriceQuote).groupBy(_._1).view.mapValues(_.map(_._2)).toMap
    } yield quotes
  }

  override def getLatestQuotes(currencies: Set[Currency]): IO[PriceQuoteError, Map[Currency, PriceQuote]] = {
    if(currencies.nonEmpty) {
      val filter = currencies.map(currency => PropertyFilter.eq("currency", currency.value))
      val query = Query.newProjectionEntityQueryBuilder()
        .setKind(datastoreConfig.currency)
        .setFilter(CompositeFilter.and(filter.head, filter.tail.toList: _*))
        .setProjection("name", "timestamp", "price")
        .setDistinctOn("name")
        .setOrderBy(OrderBy.asc("name"), OrderBy.desc("timestamp"), OrderBy.asc("price"))
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
    } else {
      UIO(Map.empty)
    }
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
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _)).toLayer
}
