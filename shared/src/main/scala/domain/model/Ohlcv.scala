package io.softwarechain.cryptojournal
package domain.model

import domain.pricequote.CurrencyAddressPair

import java.time.Instant

final case class Ohlcv(pair: CurrencyAddressPair,
                       timestamp: Instant,
                       open: BigDecimal,
                       close: BigDecimal,
                       minimum: BigDecimal,
                       median: BigDecimal,
                       maximum: BigDecimal,
                       volume: BigDecimal)
