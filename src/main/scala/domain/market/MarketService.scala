package io.softwarechain.cryptojournal
package domain.market

import error._
import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait MarketService {
  def getHistoricalOhlcv(): IO[MarketError, List[Ohlcv]]
}

final case class LiveMarketService(historicalDataRepo: HistoricalDataRepo, logger: Logger[String]) extends MarketService {
  override def getHistoricalOhlcv(): IO[MarketError, List[Ohlcv]] = historicalDataRepo.getHistoricalOhlcv()
}

object LiveMarketService {
  lazy val layer: URLayer[Has[HistoricalDataRepo] with Logging, Has[MarketService]] = (LiveMarketService(_, _)).toLayer
}