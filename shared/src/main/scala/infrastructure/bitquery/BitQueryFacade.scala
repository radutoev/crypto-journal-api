package io.softwarechain.cryptojournal
package infrastructure.bitquery

import config.BitQueryConfig
import domain.model.Ohlcv
import domain.model.date.Hour
import domain.pricequote.{CurrencyAddressPair, PriceQuote}
import infrastructure.bitquery.BitQueryFacade.BitQueryDateFormatter
import infrastrucutre.bitquery.graphql.client.BaseCurrencyEnum.USD
import infrastrucutre.bitquery.graphql.client.Ethereum.dexTrades
import infrastrucutre.bitquery.graphql.client.EthereumDexTrades._
import infrastrucutre.bitquery.graphql.client.PriceAggregateFunction.average
import infrastrucutre.bitquery.graphql.client.Query.ethereum
import infrastrucutre.bitquery.graphql.client.TimeInterval.minute
import infrastrucutre.bitquery.graphql.client._
import util.InstantOps
import vo.TimeInterval

import eu.timepit.refined.refineMV
import sttp.client3._
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

final case class BitQueryFacade (config: BitQueryConfig,
                                 logger: Logger[String]) {
  private lazy val zioHttpBackend = HttpClientZioBackend()

  /**
   * I have noticed that if we provide large time intervals, then the result is empty if doing choosing minute granularity.
   * In order to handle this limitation we split the interval in 5 days chunks.
   *
   * TODO Change the return type to ZStream.
   */
  def getPrices(pair: CurrencyAddressPair, since: Instant)(clock: Clock.Service): Task[List[PriceQuote]] = {
    def doGetPrices(interval: TimeInterval): IO[Option[String], List[PriceQuote]] = {
      val query = ethereum(network = Some(EthereumNetwork.bsc))(
        dexTrades(
          date = Some(DateSelector(between = Some(List(interval.start.toLocalDate().toString, interval.end.toLocalDate().toString)))),
          exchangeName = Some(List(StringSelector(is = Some("Pancake v2")))),
          baseCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.base.address.value)))),
          quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.quote.address.value))))
        ) {
          timeInterval {
            minute(count = Some(1), format = Some("%FT%TZ"))
          } ~
            quotePrice(calculate = Some(average))
        }
      )

      logger.info(s"Fetch quotes for $pair, interval: $interval") *>
      zioHttpBackend.flatMap { backend =>
        query
          .toRequest(uri"${config.url}", dropNullInputValues = true)
          .headers(Map("X-API-KEY" -> config.apiKey))
          .send(backend)
          .map(_.body)
          .tapError(err => logger.warn(err.getMessage))
          .map(_.map(result => {
            result.flatten.getOrElse(List.empty).collect {
              case (Some(rawTimestamp), Some(rawPrice)) => PriceQuote(rawPrice, Instant.parse(rawTimestamp))
            }
          }))
          .absolve
      }
      .orElseFail(Some("cannot fetch price quotes"))
    }

    (for {
      now     <- clock.instant
      chunks  = TimeInterval(since, now).dayChunks(refineMV(2))
      data    <- ZIO.collect(chunks)(doGetPrices)
    } yield data.flatten).orElseFail(new RuntimeException("Unable to fetch quotes"))
  }

  def getPrices(pair: CurrencyAddressPair, hour: Hour): Task[List[PriceQuote]] = {
    val timestamps = List(BitQueryDateFormatter.format(hour.value), BitQueryDateFormatter.format(hour.value.plusSeconds(3599)))
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(DateSelector(between = Some(timestamps))),
        exchangeName = Some(List(StringSelector(is = Some("Pancake v2")))),
        baseCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.base.address.value)))),
        quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.quote.address.value))))
      ) {
        timeInterval {
          minute(format = Some("%FT%TZ"))
        } ~
        quotePrice()
      }
    )

    zioHttpBackend.flatMap { backend =>
      query
        .toRequest(uri"${config.url}", dropNullInputValues = true)
        .headers(Map("X-API-KEY" -> config.apiKey))
        .send(backend)
        .map(_.body)
        .tapError(err => logger.warn(err.getMessage))
        .map(_.map(result => {
          result.flatten.getOrElse(List.empty).collect {
            case (Some(rawTimestamp), Some(rawPrice)) => PriceQuote(rawPrice, Instant.parse(rawTimestamp))
          }
        }))
        .absolve
    }.orElseFail(new RuntimeException("Unable to fetch quotes"))
  }

  def getOhlcv(pair: CurrencyAddressPair, interval: TimeInterval): Task[List[Ohlcv]] = {
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(DateSelector(since = Some(BitQueryDateFormatter.format(interval.start)), till = Some(BitQueryDateFormatter.format(interval.end)))),
        exchangeName = Some(List(StringSelector(in = Some(List("Pancake", "Pancake v2"))))),
        baseCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.base.address.value)))),
        quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.quote.address.value)))),
        options = Some(QueryOptions(limit = Some(1000)))
      ) {
        timeInterval {
          minute(format = Some("%FT%TZ"))
        } ~
        tradeAmount(in = USD, calculate = Some(AmountAggregateFunction.sum)) ~
        quotePrice(calculate = Some(PriceAggregateFunction.maximum)) ~
        quotePrice(calculate = Some(PriceAggregateFunction.minimum)) ~
        quotePrice(calculate = Some(PriceAggregateFunction.median)) ~
        minimum(of = EthereumDexTradesMeasureable.block, get = Some(EthereumDexTradesMeasureable.quote_price)) ~
        maximum(of = EthereumDexTradesMeasureable.block, get = Some(EthereumDexTradesMeasureable.quote_price))
      }
    )

    zioHttpBackend.flatMap { backend =>
      query
        .toRequest(uri"${config.url}", dropNullInputValues = true)
        .headers(Map("X-API-KEY" -> config.apiKey))
        .send(backend)
        .map(_.body)
        .tapError(err => logger.warn(err.getMessage))
        .map(_.map(result => {
          result.flatten.getOrElse(List.empty).collect {
            case ((((((Some(rawTimestamp), Some(rawTradeAmount)), Some(rawMax)), Some(rawMin)), Some(rawMedian)), Some(rawOpen)), Some(rawClose)) =>
              Ohlcv(pair,
                timestamp = Instant.parse(rawTimestamp),
                open = BigDecimal(rawOpen),
                close = BigDecimal(rawClose),
                minimum = BigDecimal(rawMax),
                median = BigDecimal(rawMedian),
                maximum = BigDecimal(rawMin),
                volume = BigDecimal(rawTradeAmount)
              )
          }
        }))
        .absolve
    }.orElseFail(new RuntimeException("Unable to fetch ohlcv data"))
  }
}

object BitQueryFacade {
  val BitQueryDateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneOffset.UTC)

  lazy val layer: URLayer[Has[BitQueryConfig] with Logging, Has[BitQueryFacade]] = (BitQueryFacade(_, _)).toLayer
}
