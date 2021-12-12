package io.softwarechain.cryptojournal
package vo

import domain.model.Currency
import domain.pricequote.CurrencyPair

import java.time.Instant

final case class CurrencyPairTimestamps(pair: CurrencyPair, timestamps: List[Instant])

final case class CurrencyPairTimestamp(pair: CurrencyPair, timestamp: Instant)

object CurrencyPairTimestamp {
  def apply(base: Currency, quote: Currency, timestamp: Instant): CurrencyPairTimestamp = {
    new CurrencyPairTimestamp(CurrencyPair(base, quote), timestamp)
  }
}

object CurrencyPairTimestamps {
  def apply(tuple: (CurrencyPair, List[Instant])): CurrencyPairTimestamps = {
    new CurrencyPairTimestamps(tuple._1, tuple._2)
  }

  def unapply(ct: CurrencyPairTimestamps): Some[(CurrencyPair, List[Instant])] = Some((ct.pair, ct.timestamps))
}
