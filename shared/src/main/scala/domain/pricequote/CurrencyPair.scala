package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency

final case class CurrencyPair(base: Currency, quote: Currency)
