package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency

final case class CurrencyPair(base: Currency, quote: Currency) {
  override def toString: String = {
    s"${base.value}-${quote.value}"
  }
}
