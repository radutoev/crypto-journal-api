package io.softwarechain.cryptojournal
package domain.position

import java.time.Instant

trait MarketPlay {
  def openedAt: Instant
}
