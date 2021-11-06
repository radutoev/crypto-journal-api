package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Currency, FungibleData, PlayId}

import java.time.Instant

trait MarketPlay {
  def id: Option[PlayId]

  def openedAt: Instant

  def fees(): Map[Currency, FungibleData]
}
