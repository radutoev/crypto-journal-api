package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency

object error {
  sealed trait PriceQuoteError
  final case class PriceQuoteFetchError(message: String)                     extends PriceQuoteError
  final case class PriceQuotesSaveError(pair: CurrencyPair, message: String) extends PriceQuoteError
  final case class PriceQuoteNotFound(currency: Currency)                    extends PriceQuoteError
  final case class PriceQuotesDeleteError(message: String)                   extends PriceQuoteError
}
