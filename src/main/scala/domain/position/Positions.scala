package io.softwarechain.cryptojournal
package domain.position

import java.time.Instant

final case class Positions(items: List[Position], lastSync: Option[Instant]) {
  def merge(other: Positions): Positions =
    Positions((items ::: other.items).sortBy(_.openedAt)(Ordering[Instant]), lastSync)
}

object Positions {
  def empty() = Positions(items = List.empty, lastSync = None)

  def apply(items: List[Position], lastSync: Option[Instant]) = new Positions(items, lastSync)

  def apply(items: List[Position]): Positions = apply(items, None)
}
