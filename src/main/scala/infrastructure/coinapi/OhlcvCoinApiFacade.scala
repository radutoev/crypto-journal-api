package io.softwarechain.cryptojournal
package infrastructure.coinapi

import domain.market.{Ohlcv, OhlcvRepo}

import sttp.client3.httpclient.zio.SttpClient
import zio.{Has, IO, URLayer}
import zio.logging.{Logger, Logging}

final case class OhlcvCoinApiFacade (httpClient: SttpClient.Service, config: CoinApiConfig, logger: Logger[String]) extends OhlcvRepo {
  override def getHistoricalData(): IO[Throwable, List[Ohlcv]] = ???
}

object OhlcvCoinApiFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[OhlcvRepo]] =
    (OhlcvCoinApiFacade(_, _, _)).toLayer
}
