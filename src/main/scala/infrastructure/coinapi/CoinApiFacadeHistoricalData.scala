package io.softwarechain.cryptojournal
package infrastructure.coinapi

import domain.market.{ HistoricalDataRepo, Ohlcv }
import domain.market.error._

import dto.HistoricalData
import dto.HistoricalData._
import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.{ Has, IO, UIO, URLayer, ZIO }
import zio.logging.{ Logger, Logging }
import zio.json._

final case class CoinApiFacadeHistoricalData(
  httpClient: SttpClient.Service,
  config: CoinApiConfig,
  logger: Logger[String]
) extends HistoricalDataRepo {
  override def getHistoricalOhlcv(): IO[MarketError, List[Ohlcv]] =
    for {
      _   <- logger.info("Fetching historical ohlcv data")
      url = s"${config.baseUrl}/ohlcv/BITSTAMP_SPOT_BTC_USD/history?period_id=1MIN&time_start=2021-09-25T00:00:00"
      response <- httpClient
                   .send(basicRequest.get(uri"$url").header("X-CoinAPI-Key", config.key))
                   .tapError(err => logger.error(s"Ohlcv historical data get error: ${err.getMessage}"))
                   .orElseFail(HistoricalDataGetError("Unable to fetch historical data"))
      data <- ZIO
               .fromEither(response.body)
               .map(_.fromJson[List[HistoricalData]].map(_.map(toDomain)))
               .flatMap(_.fold(s => ZIO.fail(s), data => UIO(data)))
               .tapError(s => logger.warn(s))
               .mapError(err => HistoricalDataGenerationError(err))
    } yield data
}

object CoinApiFacadeHistoricalData {
  val layer: URLayer[SttpClient with Has[CoinApiConfig] with Logging, Has[HistoricalDataRepo]] =
    (CoinApiFacadeHistoricalData(_, _, _)).toLayer
}
