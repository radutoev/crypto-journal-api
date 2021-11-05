package io.softwarechain.cryptojournal
package domain.position

import domain.model.{FungibleData, PlayId}

import java.time.Instant

trait MarketPlay {
  def id: Option[PlayId]

  def openedAt: Instant

  def totalFees(): Option[FungibleData]
}
