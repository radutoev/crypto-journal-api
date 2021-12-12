package io.softwarechain.cryptojournal
package infrastructure.bitquery

import config.BitQueryConfig
import domain.model.CoinAddress
import domain.pricequote.{CurrencyPair, PriceQuote}
import infrastrucutre.bitquery.graphql.client.Ethereum.dexTrades
import infrastrucutre.bitquery.graphql.client.EthereumDexTrades.{quotePrice, timeInterval}
import infrastrucutre.bitquery.graphql.client.PriceAggregateFunction.average
import infrastrucutre.bitquery.graphql.client.Query.ethereum
import infrastrucutre.bitquery.graphql.client.TimeInterval.minute
import infrastrucutre.bitquery.graphql.client.{DateSelector, EthereumCurrencySelector, EthereumNetwork, StringSelector}

import sttp.client3._
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.logging.{Logger, Logging}
import zio.{Has, Task, URLayer}

import java.time.{Instant, LocalDate}

final case class BitQueryFacade (config: BitQueryConfig,
                                 logger: Logger[String]) {
  private lazy val zioHttpBackend = HttpClientZioBackend()

  def getPrices(base: CoinAddress, quote: CoinAddress, since: LocalDate): Task[List[PriceQuote]] = {
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(DateSelector(since = Some(since.toString))),
        exchangeName = Some(List(StringSelector(is = Some("Pancake v2")))),
        baseCurrency = Some(List(EthereumCurrencySelector(is = Some(base.value)))),
        quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(quote.value))))
      ) {
        timeInterval {
          minute(count = Some(1), format = Some("%FT%TZ"))
        } ~
        quotePrice(calculate = Some(average))
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
    }
  }

  def getPrices(pair: CurrencyPair, timestamps: List[Instant]): Task[List[PriceQuote]] = {
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(DateSelector(in = Some(timestamps.map(_.toString)))),
        exchangeName = Some(List(StringSelector(is = Some("Pancake v2")))),
        baseCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.base.value)))),
        quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(pair.quote.value))))
      ) {
        timeInterval {
          minute(count = Some(1), format = Some("%FT%TZ"))
        } ~
          quotePrice(calculate = Some(average))
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
    }
  }
}

object BitQueryFacade {
  lazy val layer: URLayer[Has[BitQueryConfig] with Logging, Has[BitQueryFacade]] = (BitQueryFacade(_, _)).toLayer
}
