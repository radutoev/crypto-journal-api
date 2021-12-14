package io.softwarechain.cryptojournal
package vo

import domain.model.{CoinAddress, Currency, CurrencyAddress}
import domain.pricequote.CurrencyAddressPair

import java.time.Instant

final case class CurrencyPairTimestamps(pair: CurrencyAddressPair, timestamps: List[Instant])

final case class CurrencyPairTimestamp(pair: CurrencyAddressPair, timestamp: Instant)

object CurrencyPairTimestamp {
  def apply(base: Currency, quote: Currency, baseAddress: CoinAddress, quoteAddress: CoinAddress, timestamp: Instant): CurrencyPairTimestamp = {
    new CurrencyPairTimestamp(CurrencyAddressPair(CurrencyAddress(base, baseAddress), CurrencyAddress(quote, quoteAddress)), timestamp)
  }
}

object CurrencyPairTimestamps {
  def apply(tuple: (CurrencyAddressPair, List[Instant])): CurrencyPairTimestamps = {
    new CurrencyPairTimestamps(tuple._1, tuple._2)
  }

  def unapply(ct: CurrencyPairTimestamps): Some[(CurrencyAddressPair, List[Instant])] = Some((ct.pair, ct.timestamps))
}
