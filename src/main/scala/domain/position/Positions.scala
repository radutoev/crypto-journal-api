package io.softwarechain.cryptojournal
package domain.position

import java.time.Instant

//most recent items first.
final case class Positions(items: List[Position], lastSync: Option[Instant]) {
  def merge(other: Positions): Positions = {
    val (open, closed) = other.items.partition(_.isOpen())

    //i cannot have more than 1 position opened for a single currency, therefore I can create a map
    var currencyPositionMap = open.map(pos => pos.currency -> pos).toMap

    //oldest first
    val merged = items.reverse.map(position => {
      if(currencyPositionMap.contains(position.currency)) {
        val oldOpenPosition = currencyPositionMap(position.currency)
        oldOpenPosition.copy(
          entries = oldOpenPosition.entries ::: position.entries
        )
        currencyPositionMap -= position.currency
        oldOpenPosition
      } else {
        position
      }
    })

    val stillOpen = currencyPositionMap.values.toList

    Positions((closed ::: stillOpen ::: merged).sortBy(_.openedAt)(Ordering[Instant]), lastSync)
  }
}

object Positions {
  def empty() = Positions(items = List.empty, lastSync = None)

  def apply(items: List[Position], lastSync: Option[Instant]) = new Positions(items, lastSync)

  def apply(items: List[Position]): Positions = apply(items, None)
}
