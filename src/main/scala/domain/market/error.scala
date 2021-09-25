package io.softwarechain.cryptojournal
package domain.market

object error {
  sealed trait MarketError
  final case class HistoricalDataGetError(message: String) extends MarketError
}
