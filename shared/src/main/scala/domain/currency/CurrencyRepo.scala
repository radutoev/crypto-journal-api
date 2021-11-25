package io.softwarechain.cryptojournal
package domain.currency

import domain.currency.error.CurrencyError
import domain.model.{ CoinAddress, Currency }

import zio.IO

trait CurrencyRepo {
  def getCurrencies(): IO[CurrencyError, Set[(Currency, CoinAddress)]]

  def upsert(currencies: Set[(Currency, CoinAddress)]): IO[CurrencyError, Unit]
}
