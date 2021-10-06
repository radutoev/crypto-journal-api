package io.softwarechain.cryptojournal
package domain.market

object error {
  sealed trait MarketError
  final case class HistoricalDataGetError(message: String)        extends MarketError
  final case class HistoricalDataGenerationError(message: String) extends MarketError
}
