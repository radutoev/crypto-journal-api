package io.softwarechain.cryptojournal
package domain.currency

object error {
  sealed trait CurrencyError
  final case class CurrencyFetchError() extends CurrencyError
}
