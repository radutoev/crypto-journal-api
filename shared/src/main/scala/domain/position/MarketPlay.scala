package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Currency, FungibleData, PlayId}

import io.softwarechain.cryptojournal.vo.TimeInterval

import java.time.Instant

trait MarketPlay {
  def id: Option[PlayId]

  def openedAt: Instant

  def fees(): Map[Currency, FungibleData]

  def inInterval(interval: TimeInterval): Boolean = {
    val moreRecentThanStart = interval.start.isBefore(openedAt) || interval.start == openedAt
    val beforeEnd = interval.end.isAfter(openedAt) || interval.end == openedAt
    moreRecentThanStart && beforeEnd
  }
}
