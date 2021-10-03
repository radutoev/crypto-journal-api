package io.softwarechain.cryptojournal
package vo

import domain.model.FungibleData

final case class PeriodDistribution(
  weekly: List[FungibleData],
  monthly: List[FungibleData],
  yearly: Map[Int, FungibleData]
)
