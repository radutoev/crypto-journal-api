package io.softwarechain.cryptojournal
package domain.position

import java.time.Instant

final case class Positions(items: List[Position], lastSync: Option[Instant])

object Positions {
  def empty() = Positions(items = List.empty, lastSync = None)
}
