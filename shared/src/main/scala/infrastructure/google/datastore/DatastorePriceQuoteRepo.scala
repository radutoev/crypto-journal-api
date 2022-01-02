package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.Currency
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError, PriceQuoteNotFound}
import domain.pricequote.{CurrencyPair, PriceQuote, PriceQuoteRepo}
import infrastructure.google.datastore.DatastorePriceQuoteRepo.{PriceQuoteBase, generateDatastoreQuotes}
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
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteRepo with DatastoreOps {

  type Ancestor = (String, String)

  override def getQuotes(pair: CurrencyPair, interval: TimeInterval): IO[PriceQuoteError, List[PriceQuote]] = {
    val filter = CompositeFilter.and(
      PropertyFilter
        .ge("timestamp", Timestamp.ofTimeSecondsAndNanos(interval.start.getEpochSecond, interval.start.getNano)),
      PropertyFilter
        .le("timestamp", Timestamp.ofTimeSecondsAndNanos(interval.end.getEpochSecond, interval.end.getNano)),
      PropertyFilter.eq("quoteCurrency", pair.quote.value),
      PropertyFilter.eq("baseCurrency", pair.base.value)
    )
    logger.info(s"Fetch quotes in $interval for ${pair.base.value}") *>
      getQuotes(filter).map(_.getOrElse(pair, List.empty))
  }

  private def getQuotes(filter: StructuredQuery.Filter): IO[PriceQuoteError, Map[CurrencyPair, List[PriceQuote]]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(datastoreConfig.priceQuote)
      .setFilter(filter)
      .addOrderBy(OrderBy.asc("timestamp"))
      .build()

    executeQuery(query)(datastore, logger)
      .mapBoth(
        _ => PriceQuoteFetchError("Unable to fetch quotes"),
        results =>
          results.asScala.toList
            .map(entityToPriceQuote)
            .groupBy(_._1)
            .view
            .mapValues(_.map(_._2))
            .toMap
      )
  }

  override def getLatestQuote(currency: Currency): IO[PriceQuoteError, PriceQuote] = {
    val query = Query
      .newProjectionEntityQueryBuilder()
      .setKind(datastoreConfig.priceQuote)
      .setFilter(PropertyFilter.eq("baseCurrency", currency.value))
      .setOrderBy(OrderBy.desc("timestamp"))
      .setLimit(1)
      .build()

    executeQuery(query)(datastore, logger)
      .orElseFail(PriceQuoteFetchError("Unable to fetch latest quotes"))
      .flatMap(results => ZIO.fromOption(results.asScala.toList.headOption).orElseFail(PriceQuoteNotFound(currency)))
      .map(entity =>
        PriceQuote(
          Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
          Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
        )
      )
  }

  override def saveQuotes(quotesChunk: PriceQuotesChunk): IO[PriceQuoteError, Unit] = {
    @inline
    def saveEntities(list: List[Entity]) = {
      Task(datastore.put(list: _*))
        .tapError(throwable =>
          logger.error(s"Error saving quotes: ${list.mkString(",")}") *> logger.error(throwable.getMessage)
        )
        .ignore //TODO handle errors
    }

    def elementKind(depth: Int): Option[String] = {
      Option(depth match {
        case 0 => "DayPriceQuote"
        case 1 => "HourPriceQuote"
        case 2 => "MinutePriceQuote"
        case _ => ""
      }).filter(_.nonEmpty)
    }

    def ancestor(depth: Int, value: String): Option[Ancestor] = {
      elementKind(depth).map(_ -> value)
    }

    def makeEntities(baseQuotes: List[PriceQuoteBase], ancestors: List[Ancestor])(implicit keyFactory: KeyFactory): List[Entity] = {
      baseQuotes.flatMap { quote =>
        var keyBuilder = keyFactory.setKind(elementKind(ancestors.size).get)
        if(ancestors.nonEmpty) {
          keyBuilder = keyBuilder.addAncestors(ancestors.map(a => PathElement.of(a._1, a._2)).asJava)
        }
        val id = UUID.randomUUID().toString //TODO Key needs to be generated based on date i think.

        val childAncestor = ancestor(ancestors.size, id).get
        val entity = Entity.newBuilder(keyBuilder.newKey(id))
          .set("baseCurrency", quotesChunk.pair.base.value)
          .set("quoteCurrency", quotesChunk.pair.quote.value)
          .set("timestamp", TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(quote.timestamp.getEpochSecond, quote.timestamp.getNano)))
          .set("price", quote.price)
          .build()
        val childrenEntities = makeEntities(quote.children, ancestors :+ childAncestor)
        entity +: childrenEntities
      }
    }

    {
//      val quotesBase = generateDatastoreQuotes(quotesChunk.quotes)
      val quotesBase = quotesChunk.quotes.map(q => PriceQuoteBase(q.timestamp, q.price, Nil))
      val entities = makeEntities(quotesBase, Nil)(datastore.newKeyFactory())
        .grouped(500)
        .toList

      for {
        _ <- ZIO.foreach_(entities)(items => saveEntities(items))
        _ <- logger.info(s"Finished upsert of quotes")
      } yield ()
    }.when(quotesChunk.quotes.nonEmpty)
  }

  private val entityToPriceQuote: Entity => (CurrencyPair, PriceQuote) = entity => {
    (
      CurrencyPair(
        Currency.unsafeFrom(entity.getString("baseCurrency")),
        Currency.unsafeFrom(entity.getString("quoteCurrency"))
      ),
      PriceQuote(
        Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble),
        Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
      )
    )
  }
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _)).toLayer

  private[datastore] def generateDatastoreQuotes(quotes: List[PriceQuote]): List[PriceQuoteBase] = {
    def generateQuotesBase(source: List[PriceQuote], groupFns: List[Instant => Instant]): List[PriceQuoteBase] = {
      if(groupFns.nonEmpty) {
        val acc: mutable.Map[Instant, mutable.ArrayBuffer[PriceQuote]] = mutable.Map.empty
        source.foreach { quote =>
          val key = groupFns.head(quote.timestamp)
          acc.update(key, acc.getOrElse(key, mutable.ArrayBuffer.empty).addOne(quote))
        }
        acc.view.map { case (timestamp, innerQuotes) =>
          PriceQuoteBase(
            timestamp,
            BigDecimal(innerQuotes.map(_.price).sum / innerQuotes.length).setScale(2, BigDecimal.RoundingMode.HALF_UP).toDouble, //average of given quotes.
            generateQuotesBase(innerQuotes.toList, groupFns.tail)
          )
        }
        .toList
      } else {
        source.map(q => PriceQuoteBase(q.timestamp, q.price, Nil))
      }
    }

    val groupFns: List[Instant => Instant] = List(
      t => t.atBeginningOfDay(),
      t => t.atBeginningOfHour()
    )
    generateQuotesBase(quotes, groupFns)
  }

  private[datastore] case class PriceQuoteBase(timestamp: Instant, price: Double, children: List[PriceQuoteBase])
}
