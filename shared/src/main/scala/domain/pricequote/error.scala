package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.CoinAddress

object error {
  sealed trait PriceQuoteError
  final case class PriceQuoteFetchError(message: String)     extends PriceQuoteError
  final case class PriceQuoteNotFound(contract: CoinAddress) extends PriceQuoteError
}
