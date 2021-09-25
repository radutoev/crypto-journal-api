package io.softwarechain.cryptojournal
package domain.market

import zio.IO

/**
 * OHLCV (Open, High, Low, Close, Volume) timeseries data.
 */
trait OhlcvRepo {
  def getHistoricalData(): IO[Throwable, List[Ohlcv]]
}
