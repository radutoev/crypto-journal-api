package io.softwarechain.cryptojournal
package domain.market

import domain.model.FungibleData

import java.time.Instant

/**
 * @param timePeriodStart Period starting time (range left inclusive)
 * @param timePeriodEnd Period ending time (range right exclusive)
 * @param timeOpen Time of first trade inside period range
 * @param timeClose Time of last trade inside period range
 * @param priceOpen First trade price inside period range
 * @param priceHigh Highest traded price inside period range
 * @param priceLow Lowest traded price inside period range
 * @param priceClose Last trade price inside period range
 */
final case class Ohlcv(
  timePeriodStart: Instant,
  timePeriodEnd: Instant,
  timeOpen: Instant,
  timeClose: Instant,
  priceOpen: FungibleData,
  priceHigh: FungibleData,
  priceLow: FungibleData,
  priceClose: FungibleData
)
