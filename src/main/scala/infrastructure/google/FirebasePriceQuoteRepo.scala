package io.softwarechain.cryptojournal
package infrastructure.google

import domain.pricequote.{ PriceQuote, PriceQuoteRepo }
import util.InstantOps
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{ CompositeFilter, OrderBy, PropertyFilter }
import com.google.cloud.datastore._
import zio.{ Function1ToLayerSyntax, Has, Task, UIO, URLayer }

import java.time.Instant
import scala.jdk.CollectionConverters._

final case class FirebasePriceQuoteRepo(datastore: Datastore) extends PriceQuoteRepo {
  override def getQuotes(interval: TimeInterval): Task[List[PriceQuote]] =
    for {
      results <- Task {
                  val start    = interval.start.resetHourAndMinute()
                  val maybeEnd = interval.end.map(_.resetHourAndMinute())
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
                    .setKind("PriceQuotes")
                    .setFilter(filter)
                    .addOrderBy(OrderBy.desc("timestamp"))
                    .build()
                  val queryResults: QueryResults[Entity] = datastore.run(query, Seq.empty[ReadOption]: _*)
                  queryResults
                }.tapError(errr => UIO(println(errr)))
      priceQuotes = results.asScala.toList.map(entityToPriceQuote)
    } yield priceQuotes

  private val entityToPriceQuote: Entity => PriceQuote = entity => {
    PriceQuote(entity.getDouble("price").toFloat, Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds))
  }
}

object FirebasePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore], Has[PriceQuoteRepo]] = (FirebasePriceQuoteRepo(_)).toLayer
}
