package io.softwarechain.cryptojournal
package domain.currency

import domain.currency.error.CurrencyError
import domain.model.Currency

import zio.IO

trait CurrencyRepo {
  def getCurrencies(): IO[CurrencyError, Set[Currency]]
}