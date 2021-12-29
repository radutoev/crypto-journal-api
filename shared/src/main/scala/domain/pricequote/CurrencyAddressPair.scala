package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.CurrencyAddress

final case class CurrencyAddressPair(base: CurrencyAddress, quote: CurrencyAddress)
