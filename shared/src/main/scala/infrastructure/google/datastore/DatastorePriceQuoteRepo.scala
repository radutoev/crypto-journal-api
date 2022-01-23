package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.Currency
import domain.model.date._
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError, PriceQuoteNotFound, PriceQuotesSaveError}
import domain.pricequote.{CurrencyPair, PriceQuote, PriceQuoteRepo}
import infrastructure.google.datastore.DatastorePriceQuoteRepo.{AveragingPriceQuote, PriceQuoteBase, TimeUnitInstanceOps, TimeUnitOps, generateDatastoreQuotes}
import util.ListOptionOps
import vo.{PriceQuotesChunk, TimeInterval}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{CompositeFilter, OrderBy, PropertyFilter}
import com.google.cloud.datastore._
import com.google.datastore.v1.QueryResultBatch.MoreResultsType
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Chunk, Has, IO, Ref, Task, UIO, URLayer, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}
import scala.collection.mutable
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteRepo
    with DatastoreOps {

  type Ancestor = (String, String)

  def getQuotes(pair: CurrencyPair, minutes: Set[Minute]): IO[PriceQuoteError, List[PriceQuote]] =
    get(
      minutes
        .map(minute => minute.datastoreKey(pair))
        .map(key =>
          datastore
            .newKeyFactory()
            .setKind(envAwareKind(MinuteUnit.datastoreKind))
            .newKey(key)
        )
    )(datastore, logger)
      .mapBoth(
        _ => PriceQuoteFetchError(s"Error fetching price quotes for $pair"),
        entityQuotes => entityQuotes.view.map(entityToPriceQuote).map(_._2).toList
      )

  override def getQuotes(
    pair: CurrencyPair,
    interval: TimeInterval,
    unit: TimeUnit
  ): IO[PriceQuoteError, List[PriceQuote]] = {
    val query = Query
      .newEntityQueryBuilder()
      .setKind(envAwareKind(unit.datastoreKind))
      .setFilter(
        CompositeFilter.and(
          PropertyFilter
            .ge("timestamp", Timestamp.ofTimeSecondsAndNanos(interval.start.getEpochSecond, interval.start.getNano)),
          PropertyFilter
            .le("timestamp", Timestamp.ofTimeSecondsAndNanos(interval.end.getEpochSecond, interval.end.getNano)),
          PropertyFilter.eq("quoteCurrency", pair.quote.value),
          PropertyFilter.eq("baseCurrency", pair.base.value)
        )
      )
      .build()

    quoteStream(query)
      .runCollect
      .map(_.toList)
  }

  override def getQuotes(
    currencies: Set[Currency],
    targetCurrency: Currency,
    interval: TimeInterval,
    unit: TimeUnit
  ): IO[PriceQuoteError, List[(CurrencyPair, PriceQuote)]] = {
    val kind = envAwareKind(unit.datastoreKind)

    val timePoints: List[TimeUnitInstance] = unit match {
      case DayUnit => interval.days().map(Day(_))
      case _       => List.empty
    }

    val keys = currencies.flatMap { currency =>
      timePoints.map { timePoint =>
        timePoint.datastoreKey(CurrencyPair(currency, targetCurrency))
      }
    }.map { keyValue =>
      datastore.newKeyFactory().setKind(kind).newKey(keyValue)
    }

    //TODO I might need to change the signature such that I know what data I am missing.
    get(keys)(datastore, logger)
      .mapBoth(
        _ => PriceQuoteFetchError("Error fetching quotes"),
        _.view.map(entityToPriceQuote).toList
      )
  }

  private val DatastoreContinueResultTypes = Set(
    MoreResultsType.NOT_FINISHED,
    MoreResultsType.MORE_RESULTS_AFTER_CURSOR,
    MoreResultsType.MORE_RESULTS_AFTER_LIMIT
  )

  private def quoteStream(query: EntityQuery): ZStream[Any, PriceQuoteError, PriceQuote] = {
    ZStream {
      for {
        stateRef <- Ref.make((false, Option.empty[Cursor])).toManaged_
        pull     = stateRef.get.flatMap { case (streamEnded, cursor) =>
          if(streamEnded) {
            IO.fail(None)
          } else {
            val finalQuery = cursor.fold(query)(c => query.toBuilder.setStartCursor(c).build())
            val getPlaysEffect = for {
              results <- executeQuery(finalQuery)(datastore, logger).orElseFail(PriceQuoteFetchError("Error retrieving quotes"))
            } yield results

            getPlaysEffect
              .mapError(Some(_))
              .flatMap { result =>
                val data       = result.asScala.toList.map(entityToPriceQuote).map(_._2)
                val queryState = result.getMoreResults
                val endStream  = !DatastoreContinueResultTypes.contains(queryState) || data.isEmpty
                val cursor     = result.getCursorAfter
                stateRef.set((endStream, Some(cursor))) *> UIO(Chunk.fromIterable(data))
              }
          }
        }
      } yield pull
    }
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

    def makeEntities(baseQuotes: List[PriceQuoteBase], ancestors: List[Ancestor]): List[Entity] = {
      if(baseQuotes.nonEmpty) {
        val ancestorPaths = ancestors.map(t => PathElement.of(t._1, t._2))
        baseQuotes.flatMap { base =>
          val kind = envAwareKind(base.timeUnitInstance.unit.datastoreKind)
          val id = base.timeUnitInstance.datastoreKey(quotesChunk.pair)
          val entity = Entity
            .newBuilder(datastore.newKeyFactory().addAncestors(ancestorPaths.asJava).setKind(kind).newKey(id))
            .set("baseCurrency", quotesChunk.pair.base.value)
            .set("quoteCurrency", quotesChunk.pair.quote.value)
            .set(
              "timestamp",
              TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(base.timeUnitInstance.value.getEpochSecond, base.timeUnitInstance.value.getNano))
            )
            .set("price", base.price)
            .build()
          entity +: makeEntities(base.children, ancestors :+ (kind -> id))
        }
      } else Nil
    }

    //I also need to know which keys were not found; for those i need to create DayPriceQuotes.
    def groupByEntityPresence(entities: List[Entity],
                              allKeys: List[String]): (List[Entity], List[String]) = {
      val keys    = entities.map(_.getKey.getName)
      val newKeys = allKeys.diff(keys)
      (entities, newKeys)
    }

    def entitiesToCreate(newItems: List[PriceQuoteBase]): List[Entity] = {
      newItems.flatMap { base =>
        val id = base.timeUnitInstance.datastoreKey(quotesChunk.pair)
        val kind = envAwareKind(base.timeUnitInstance.unit.datastoreKind)
        val children = makeEntities(base.children, List(kind -> id))
        val entity = Entity
          .newBuilder(datastore.newKeyFactory().setKind(kind).newKey(id))
          .set("baseCurrency", quotesChunk.pair.base.value)
          .set("quoteCurrency", quotesChunk.pair.quote.value)
          .set(
            "timestamp",
            TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(base.timeUnitInstance.value.getEpochSecond, base.timeUnitInstance.value.getNano))
          )
          .set("price", base.price)
          .set("units",
            ListValue.newBuilder()
              .set(children
                .filter(_.getKey.getKind == envAwareKind(HourUnit.datastoreKind))
                .map(e => EntityValue.of(e))
                .asJava
              )
              .build()
          )
          .build()
        entity +: children
      }
    }

    def entitiesToUpdate(items: List[(Entity, PriceQuoteBase)]): List[Entity] = {
      items.map { case (entity, quote) =>
        val averagingPriceQuote = entityToAvgPriceQuote(entity)
        val timestamps = averagingPriceQuote.units.map(_.timestamp)
        val hoursToAdd = quote.children.filterNot(base => timestamps.contains(base.timeUnitInstance.value))
        if(hoursToAdd.nonEmpty) {
          val children = makeEntities(hoursToAdd, List(entity.getKey.getKind -> entity.getKey.getName))
          val units = averagingPriceQuote.units ++ hoursToAdd.map(base => PriceQuote(base.price, base.timeUnitInstance.value))
          val newPrice = BigDecimal(units.map(_.price).sum / units.length)
            .setScale(2, BigDecimal.RoundingMode.HALF_UP)
            .toDouble
          val entityUnits = new java.util.ArrayList(entity.getList[EntityValue]("units"))
          entityUnits.addAll(children.filter(_.getKey.getKind == envAwareKind(HourUnit.datastoreKind)).map(e => EntityValue.of(e)).asJava)
          val toUpdate = Entity.newBuilder(entity)
            .set("price", newPrice)
            .set("units", entityUnits)
            .build()
          Some(toUpdate +: children)
        } else {
          None
        }
      }.values.flatten
    }

    def makeDayKey(key: String): Key =
      datastore.newKeyFactory()
        .setKind(envAwareKind(DayUnit.datastoreKind))
        .newKey(key)

    {
      implicit val keyFactory: KeyFactory = datastore.newKeyFactory()
      val quotesBase = generateDatastoreQuotes(quotesChunk.quotes).toVector

      //Get Day datastore entities
      //Update entities if needed with new price value and
      //write hourly entities (this is an atomic operation, because I have all the minutes for a given hour).

      val keysAndIndices = quotesBase.zipWithIndex.map { case (quotesBase, index) =>
        quotesBase.timeUnitInstance.datastoreKey(quotesChunk.pair) -> index
      }.toMap

      val keys = keysAndIndices.map { case (key, _) => makeDayKey(key) }.toList

      val upsertEffect = for {
        results             <- get(keys.toSet)(datastore, logger)
        (toUpdate, newKeys) = groupByEntityPresence(results, keys.map(_.getName))
        newQuotes           = newKeys.map(newKey => keysAndIndices.get(newKey).map(index => quotesBase(index))).values
        newEntities         = entitiesToCreate(newQuotes)
        toUpdateBaseQuotes  = toUpdate.map { entity =>
          keysAndIndices.get(entity.getKey.getName).map(index => quotesBase(index)).map(base => entity -> base)
        }.values
        updateEntities      = entitiesToUpdate(toUpdateBaseQuotes)
        _                   <- logger.info(s"New entities: ${newEntities.size}, update: ${updateEntities.size}")
        upsertEntities      = newEntities ++ updateEntities
        _                   <- ZIO.foreach_(upsertEntities.grouped(100).toList)(items => saveEntities(items))
      } yield ()

      upsertEffect.orElseFail(PriceQuotesSaveError(quotesChunk.pair, "Unable to save price quotes"))
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

  private def entityToAvgPriceQuote(entity: Entity): AveragingPriceQuote = {
    AveragingPriceQuote(
      quote = PriceQuote(entity.getDouble("price"), Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)),
      units = entity.getList[EntityValue]("units").asScala.toList.map { ev =>
        PriceQuote(
          price = ev.get().getDouble("price"),
          timestamp = Instant.ofEpochSecond(ev.get().getTimestamp("timestamp").getSeconds)
        )
      }
    )
  }

  private def envAwareKind(base: String): String = {
    val testEnv = datastoreConfig.priceQuote.toLowerCase.endsWith("test")
    if (testEnv) {
      base + "Test"
    } else base
  }
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _)).toLayer

  private[datastore] def generateDatastoreQuotes(quotes: List[PriceQuote]): List[PriceQuoteBase] = {
    def generateQuotesBase(source: List[PriceQuote], groupFns: List[Instant => TimeUnitInstance]): List[PriceQuoteBase] =
      if (groupFns.nonEmpty) {
        val acc: mutable.Map[TimeUnitInstance, mutable.ArrayBuffer[PriceQuote]] = mutable.Map.empty
        source.foreach { quote =>
          val key = groupFns.head(quote.timestamp)
          acc.update(key, acc.getOrElse(key, mutable.ArrayBuffer.empty).addOne(quote))
        }
        acc.view.map {
          case (timestamp, innerQuotes) =>
            PriceQuoteBase(
              timestamp,
              BigDecimal(innerQuotes.map(_.price).sum / innerQuotes.length)
                .setScale(2, BigDecimal.RoundingMode.HALF_UP)
                .toDouble, //average of given quotes.
              generateQuotesBase(innerQuotes.toList, groupFns.tail)
            )
        }.toList
      } else {
        //we default here to the lowest time unit, which is Minute.
        source.map(q => PriceQuoteBase(Minute(q.timestamp), q.price, Nil))
      }

    val groupFns: List[Instant => TimeUnitInstance] = List(
      t => Day(t),
      t => Hour(t)
    )
    generateQuotesBase(quotes, groupFns)
  }

  private[datastore] final case class PriceQuoteBase(timeUnitInstance: TimeUnitInstance, price: Double, children: List[PriceQuoteBase])

  implicit class TimeUnitInstanceOps(instance: TimeUnitInstance) {
    private lazy val Formatter: DateTimeFormatter =
      DateTimeFormatter.ofPattern("yyyy-MM-dd-HH-mm").withZone(ZoneOffset.UTC)

    def datastoreKey(pair: CurrencyPair): String =
      s"$pair-${Formatter.format(instance.value)}"
  }

  implicit class TimeUnitOps(unit: TimeUnit) {
    lazy val datastoreKind: String = unit match {
      case DayUnit    => "DayPriceQuote"
      case HourUnit   => "HourPriceQuote"
      case MinuteUnit => "MinutePriceQuote"
    }
  }

  private[datastore] final case class AveragingPriceQuote(quote: PriceQuote, units: List[PriceQuote])

  implicit class AveragingPriceQuoteOps(avgPriceQuote: AveragingPriceQuote) {
    def contains(hour: Hour): Boolean = {
      avgPriceQuote.units.map(_.timestamp).contains(hour.value)
    }

    def containsNot(hour: Hour): Boolean = !contains(hour)
  }
}
