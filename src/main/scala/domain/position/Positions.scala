package io.softwarechain.cryptojournal
package domain.position

import domain.model.Currency

import java.time.Instant
import scala.collection.mutable.ArrayBuffer

//most recent items first.
final case class Positions(items: List[Position], lastSync: Option[Instant]) {
  lazy val closedPositions: List[Position] = items.filter(_.isClosed())
  lazy val openPositions: List[Position]   = items.filter(_.isOpen())

  def merge(other: Positions): Positions = {
    var currencyPositionMap = Map.empty[Currency, Position]
    val otherItems          = ArrayBuffer.empty[Position]

    other.items.foreach { position =>
      if (currencyPositionMap.contains(position.currency)) {
        otherItems.addOne(position)
      } else {
        currencyPositionMap += position.currency -> position
      }
    }

    //oldest first
    val merged = items.reverse.map { position =>
      if (currencyPositionMap.contains(position.currency)) {
        val oldPosition = currencyPositionMap(position.currency)
        oldPosition.copy(
          entries = oldPosition.entries ::: position.entries
        )
        currencyPositionMap -= position.currency
        oldPosition
      } else {
        position
      }
    }

    val notCorrelated = currencyPositionMap.values.toList

    Positions((otherItems.toList ::: notCorrelated ::: merged).sortBy(_.openedAt)(Ordering[Instant]), lastSync)
  }
}

object Positions {
  def empty() = Positions(items = List.empty, lastSync = None)

  def apply(items: List[Position], lastSync: Option[Instant]) = new Positions(items, lastSync)

  def apply(items: List[Position]): Positions = apply(items, None)
}
