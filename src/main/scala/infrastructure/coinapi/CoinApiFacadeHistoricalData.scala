package io.softwarechain.cryptojournal
package infrastructure.coinapi

import domain.market.{ Ohlcv, HistoricalDataRepo }
import domain.market.error._

import sttp.client3.httpclient.zio.SttpClient
import zio.{ Has, IO, URLayer }
import zio.logging.{ Logger, Logging }

final case class CoinApiFacadeHistoricalData(httpClient: SttpClient.Service, config: CoinApiConfig, logger: Logger[String])
    extends HistoricalDataRepo {
  override def getHistoricalOhlcv(): IO[MarketError, List[Ohlcv]] = ???
}

object CoinApiFacadeHistoricalData {
  val layer: URLayer[SttpClient with Has[CoinApiConfig] with Logging, Has[HistoricalDataRepo]] =
    (CoinApiFacadeHistoricalData(_, _, _)).toLayer
}
