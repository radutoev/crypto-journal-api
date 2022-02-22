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
import vo.TimeInterval

import eu.timepit.refined.refineMV
import sttp.client3._
import sttp.client3.httpclient.zio.HttpClientZioBackend
import sttp.model.StatusCode
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Chunk, Has, IO, Task, UIO, URLayer, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Instant, ZoneOffset}

final case class BitQueryFacade(config: BitQueryConfig, logger: Logger[String]) {
  private lazy val zioHttpBackend = HttpClientZioBackend()

  /**
   * I have noticed that if we provide large time intervals, then the result is empty if doing choosing minute granularity.
   * In order to handle this limitation we split the interval in 5 days chunks.
   *
   * TODO Change the return type to ZStream.
   */
  def getPrices(pair: CurrencyAddressPair, since: Instant)(clock: Clock.Service): Task[List[PriceQuote]] = {
    def doGetPrices(interval: TimeInterval, apiKey: String): IO[Option[String], List[PriceQuote]] = {
      val query = ethereum(network = Some(EthereumNetwork.bsc))(
        dexTrades(
          date = Some(
            DateSelector(between =
              Some(List(BitQueryDateFormatter.format(interval.start), BitQueryDateFormatter.format(interval.end)))
            )
          ),
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
      zioHttpBackend.mapError(throwable => s"Backend failure ${throwable.getMessage}").flatMap { backend =>
        query
          .toRequest(uri"${config.url}", dropNullInputValues = true)
          .headers(Map("X-API-KEY" -> apiKey))
          .send(backend)
          .flatMap { response =>
            response.code match {
              case StatusCode.Ok => UIO(response)
              case StatusCode.TooManyRequests =>
                logger.warn(s"BitQuery rate limit reached on $apiKey") *> ZIO.fail("Rate Limit")
              case _ => logger.warn(s"Unexpected BitQuery response code ${response.code}") *> ZIO.fail("Unsupported")
            }
          }
          .orElseFail("Request failure")
          .map(_.body)
          .flatMap(calibanErrOrResponse =>
            ZIO.fromEither(calibanErrOrResponse).tapError(err => logger.warn(err.getMessage)).mapError(_.getMessage())
          )
          .flatMap { result =>
            val responseBody = result.flatten.getOrElse(List.empty)
            logger.info(s"Found ${responseBody.size} quotes for $interval") *>
            UIO {
              responseBody.collect {
                case (Some(rawTimestamp), Some(rawPrice)) => PriceQuote(rawPrice, Instant.parse(rawTimestamp))
              }
            }
          }
          .tapError(err => logger.warn(err))
      }.orElseFail(Some("cannot fetch price quotes"))
    }

    def getPricesEffect(interval: TimeInterval) =
      doGetPrices(interval, config.getKey()).catchSome {
        case Some("Rate Limit") => doGetPrices(interval, config.getKey())
      }

    (for {
      now    <- clock.instant
      chunks = TimeInterval(since, now).dayChunks(refineMV(2))
      data   <- ZIO.collect(chunks)(getPricesEffect)
    } yield data.flatten).orElseFail(new RuntimeException("Unable to fetch quotes"))
  }

  def pricesStream(pair: CurrencyAddressPair, since: Instant)(
    clock: Clock.Service
  ): ZStream[Any, RuntimeException, Chunk[PriceQuote]] = {
    def doGetPrices(interval: TimeInterval, apiKey: String): IO[String, Chunk[PriceQuote]] = {
      val query = ethereum(network = Some(EthereumNetwork.bsc))(
        dexTrades(
          date = Some(
            DateSelector(between =
              Some(List(BitQueryDateFormatter.format(interval.start), BitQueryDateFormatter.format(interval.end)))
            )
          ),
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
      zioHttpBackend.mapError(throwable => s"Backend failure ${throwable.getMessage}").flatMap { backend =>
        query
          .toRequest(uri"${config.url}", dropNullInputValues = true)
          .headers(Map("X-API-KEY" -> apiKey))
          .send(backend)
          .flatMap { response =>
            response.code match {
              case StatusCode.Ok => UIO(response)
              case StatusCode.TooManyRequests =>
                logger.warn(s"BitQuery rate limit reached on $apiKey") *> ZIO.fail("Rate Limit")
              case _ => logger.warn(s"Unexpected BitQuery response code ${response.code}") *> ZIO.fail("Unsupported")
            }
          }
          .orElseFail("Request failure")
          .map(_.body)
          .flatMap(calibanErrOrResponse =>
            ZIO.fromEither(calibanErrOrResponse).tapError(err => logger.warn(err.getMessage)).mapError(_.getMessage())
          )
          .flatMap { result =>
            val responseBody = result.flatten.getOrElse(List.empty)
            logger.info(s"Found ${responseBody.size} quotes for $interval") *>
            UIO {
              Chunk.fromIterable(
                responseBody.collect {
                  case (Some(rawTimestamp), Some(rawPrice)) => PriceQuote(rawPrice, Instant.parse(rawTimestamp))
                }
              )
            }
          }
          .tapError(err => logger.warn(err))
      }
    }

    def getPricesEffect(interval: TimeInterval) =
      doGetPrices(interval, config.getKey()).catchSome {
        case "Rate Limit" => doGetPrices(interval, config.getKey())
      }

    ZStream
      .fromIterable(TimeInterval(since, Instant.now()).dayChunks(refineMV(2)))
      .mapConcatChunkM(interval => getPricesEffect(interval).orElseFail(new RuntimeException("shit")))
      .grouped(2000)
  }

  def getPrices(pair: CurrencyAddressPair, hour: Hour): Task[List[PriceQuote]] = {
    val timestamps =
      List(BitQueryDateFormatter.format(hour.value), BitQueryDateFormatter.format(hour.value.plusSeconds(3599)))
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

    def doGetPrices(apiKey: String) = {
      zioHttpBackend.mapError(throwable => s"Backend failure ${throwable.getMessage}").flatMap { backend =>
        query
          .toRequest(uri"${config.url}", dropNullInputValues = true)
          .headers(Map("X-API-KEY" -> apiKey))
          .send(backend)
          .flatMap { response =>
            response.code match {
              case StatusCode.Ok => UIO(response)
              case StatusCode.TooManyRequests =>
                logger.warn(s"BitQuery rate limit reached on $apiKey") *> ZIO.fail("Rate Limit")
              case _ => logger.warn(s"Unexpected BitQuery response code ${response.code}") *> ZIO.fail("Unsupported")
            }
          }
          .orElseFail("Request failure")
          .map(_.body)
          .flatMap(calibanErrOrResponse =>
            ZIO.fromEither(calibanErrOrResponse).tapError(err => logger.warn(err.getMessage)).mapError(_.getMessage())
          )
          .flatMap { result =>
            val responseBody = result.flatten.getOrElse(List.empty)
            logger.info(s"Found ${responseBody.size} quotes for $hour") *>
              UIO {
                responseBody.collect {
                  case (Some(rawTimestamp), Some(rawPrice)) => PriceQuote(rawPrice, Instant.parse(rawTimestamp))
                }
              }
          }
          .tapError(err => logger.warn(err))
      }
    }

    doGetPrices(config.getKey()).catchSome {
      case "Rate Limit" => doGetPrices(config.getKey())
    }.orElseFail(new RuntimeException("Price fetch error"))
  }

  def getOhlcv(pair: CurrencyAddressPair, interval: TimeInterval): Task[List[Ohlcv]] = {
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(
          DateSelector(
            since = Some(BitQueryDateFormatter.format(interval.start)),
            till = Some(BitQueryDateFormatter.format(interval.end))
          )
        ),
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
        .headers(Map("X-API-KEY" -> config.getKey()))
        .send(backend)
        .map(_.body)
        .flatMap(calibanErrOrResponse =>
          ZIO.fromEither(calibanErrOrResponse).tapError(err => logger.warn(err.getMessage))
        )
        .flatMap { result =>
          val responseBody = result.flatten.getOrElse(List.empty)
          logger.info(s"Found ${responseBody.size} quotes for $interval") *>
          UIO {
            responseBody.collect {
              case (
                  (
                    ((((Some(rawTimestamp), Some(rawTradeAmount)), Some(rawMax)), Some(rawMin)), Some(rawMedian)),
                    Some(rawOpen)
                  ),
                  Some(rawClose)
                  ) =>
                Ohlcv(
                  pair,
                  timestamp = Instant.parse(rawTimestamp),
                  open = BigDecimal(rawOpen),
                  close = BigDecimal(rawClose),
                  minimum = BigDecimal(rawMax),
                  median = BigDecimal(rawMedian),
                  maximum = BigDecimal(rawMin),
                  volume = BigDecimal(rawTradeAmount)
                )
            }
          }
        }
    }.orElseFail(new RuntimeException("Unable to fetch ohlcv data"))
  }
}

object BitQueryFacade {
  val BitQueryDateFormatter = DateTimeFormatter.ofPattern("yyyy-MM-dd'T'HH:mm:ss").withZone(ZoneOffset.UTC)

  lazy val layer: URLayer[Has[BitQueryConfig] with Logging, Has[BitQueryFacade]] = (BitQueryFacade(_, _)).toLayer
}
