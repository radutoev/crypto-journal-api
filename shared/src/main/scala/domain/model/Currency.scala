package io.softwarechain.cryptojournal
package domain.model

import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV

object Currency {
  def apply(str: String): Either[String, Currency] = refineV[CurrencyPredicate](str)

  def unsafeFrom(str: String): Refined[String, CurrencyPredicate] = refineV[CurrencyPredicate].unsafeFrom(str)
}

final case class CurrencyAddress(currency: Currency, address: CoinAddress)

object currencyops {
  implicit class CurrencyOps(currency: Currency) {
    def sameCurrency(other: Currency): Boolean = currency.value == other.value
  }
}
