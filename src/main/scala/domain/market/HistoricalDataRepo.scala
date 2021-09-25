package io.softwarechain.cryptojournal
package domain.market

import error.MarketError
import zio.IO

/**
 * OHLCV (Open, High, Low, Close, Volume) timeseries data.
 */
trait HistoricalDataRepo {
  def getHistoricalOhlcv(): IO[MarketError, List[Ohlcv]]
}
