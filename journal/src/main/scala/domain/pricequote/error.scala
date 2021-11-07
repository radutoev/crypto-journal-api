package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.WalletAddress

object error {
  sealed trait PriceQuoteError
  final case class PriceQuoteFetchError(message: String) extends PriceQuoteError
  final case class PriceQuoteNotFound(contract: WalletAddress) extends PriceQuoteError
}
