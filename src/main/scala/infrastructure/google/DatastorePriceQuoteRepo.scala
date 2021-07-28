package io.softwarechain.cryptojournal
package infrastructure.google

import domain.pricequote.{ PriceQuote, PriceQuoteRepo }
import util.InstantOps
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore._
import zio.{ Function2ToLayerSyntax, Has, Task, UIO, URLayer }

import java.time.Instant
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(datastore: Datastore, datastoreConfig: DatastoreConfig)
    extends PriceQuoteRepo {
  override def getQuotes(interval: TimeInterval): Task[List[PriceQuote]] =
    for {
      results <- Task {
                  val start    = interval.start.atBeginningOfDay()
                  val maybeEnd = interval.end.atBeginningOfDay()
                  val filter =
                    PropertyFilter.ge("timestamp", Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano))
//                  val filter: StructuredQuery.Filter = if (maybeEnd.isDefined) {
//                    val end = maybeEnd.get
//                    CompositeFilter.and(
//                      PropertyFilter.ge(
//                        "timestamp",
//                        Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano)
//                      ),
//                      PropertyFilter
//                        .le("timestamp", Timestamp.ofTimeSecondsAndNanos(end.getEpochSecond, end.getNano))
//                    )
//                  } else {
//                    PropertyFilter
//                      .ge("timestamp", Timestamp.ofTimeSecondsAndNanos(start.getEpochSecond, start.getNano))
//                  }
                  val query: Query[Entity] = Query
                    .newEntityQueryBuilder()
                    .setKind(datastoreConfig.priceQuote)
                    .setFilter(filter)
                    .addOrderBy(OrderBy.desc("timestamp"))
                    .build()
                  val queryResults: QueryResults[Entity] = datastore.run(query, Seq.empty[ReadOption]: _*)
                  queryResults
                }.tapError(errr => UIO(println(errr)))
      priceQuotes = results.asScala.toList.map(entityToPriceQuote)
    } yield priceQuotes

  private val entityToPriceQuote: Entity => PriceQuote = entity => {
    PriceQuote(
      Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
      Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
    )
  }
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig], Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _)).toLayer
}
