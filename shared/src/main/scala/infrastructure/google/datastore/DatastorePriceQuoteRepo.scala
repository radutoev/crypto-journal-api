package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.{CovalentConfig, DatastoreConfig}
import domain.model.{CoinAddress, Currency}
import domain.pricequote.error.{PriceQuoteError, PriceQuoteFetchError, PriceQuoteNotFound}
import domain.pricequote.{PriceQuote, PriceQuoteRepo}
import infrastructure.google.datastore.DatastorePriceQuoteRepo.CovalentQParamDateFormat
import infrastructure.google.datastore.dto.dto.PriceQuoteResponse
import util.InstantOps
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.{OrderBy, PropertyFilter}
import com.google.cloud.datastore._
import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.{UriContext, asString, basicRequest}
import zio.clock.Clock
import zio.json.DecoderOps
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, UIO, URLayer, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneId, ZoneOffset}
import scala.jdk.CollectionConverters._
import scala.util.Try

final case class DatastorePriceQuoteRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  httpClient: SttpClient.Service,
  covalentConfig: CovalentConfig,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteRepo {
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
      priceQuotes = results.asScala.toList.map(entityToPriceQuote).sortBy(_.timestamp)(Ordering[Instant])
    } yield priceQuotes

  override def getQuotes(currencies: Set[Currency], interval: TimeInterval): IO[PriceQuoteError, Map[Currency, List[PriceQuote]]] = {
    for {
      _     <- logger.info(s"Fetch quotes in time interval $interval for currencies ${currencies.mkString(",")}")
//      query = Query.newEntityQueryBuilder().setKind(datastoreConfig.priceQuote).setFilter()
    } yield Map.empty
  }

  override def getCurrentQuote(contract: CoinAddress): IO[PriceQuoteError, PriceQuote] =
    for {
      now <- clock.instant
      day = CovalentQParamDateFormat.format(now)
      _   <- logger.info(s"Fetching quote for ${contract.value} at ${now.toString}")
      url = s"${covalentConfig.baseUrl}/v1/pricing/historical_by_addresses_v2/56/USD/${contract.value}/?from=$day&to=$day&key=${covalentConfig.key}"
      response <- httpClient
                   .send(basicRequest.get(uri"$url").response(asString))
                   .tapError(t => logger.warn(s"Covalent price quote request failed: $t"))
                   .mapError(t => PriceQuoteFetchError(t.getMessage))
      decoded <- ZIO
        .fromEither(response.body)
        .orElseFail(PriceQuoteNotFound(contract))
        .flatMap(r => ZIO.fromEither(r.fromJson[PriceQuoteResponse]).mapError(PriceQuoteFetchError))
      price <- ZIO
        .fromOption(decoded.data).orElseFail(PriceQuoteNotFound(contract))
        .flatMap(quote => ZIO.fromOption(quote.prices.headOption).orElseFail(PriceQuoteNotFound(contract)))
                .mapBoth(_ => PriceQuoteNotFound(contract), priceData => priceData.price)
    } yield PriceQuote(price, now.atBeginningOfDay())

  private val entityToPriceQuote: Entity => PriceQuote = entity => {
    PriceQuote(
      Try(entity.getDouble("price")).getOrElse(entity.getLong("price").toDouble).toFloat,
      Instant.ofEpochSecond(entity.getTimestamp("timestamp").getSeconds)
    )
  }
}

object DatastorePriceQuoteRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with SttpClient with Has[CovalentConfig] with Clock with Logging, Has[PriceQuoteRepo]] =
    (DatastorePriceQuoteRepo(_, _, _, _, _, _)).toLayer

  val CovalentQParamDateFormat = DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.from(ZoneOffset.UTC))
}
