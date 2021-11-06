package io.softwarechain.cryptojournal
package vo

import domain.model.fungible.FungibleDataOps
import domain.model.{Currency, FungibleData}

/**
 * Semantically this represents the value of a "thing" in multiple currencies.
 * Its existence id due to the fact that we want to display all currencies of something, be it fee or cost, at once.
 * If there were a dynamic choice of currency and the quotes for the requested currency were obtained as en effect, then this value object would not make sense.
 *
 * This ennsu
 */
final case class FungibleDataGroup (private val currencyData: Map[Currency, FungibleData]) {
  def get(currency: Currency): Option[FungibleData] = ???
}

object FungibleDataGroup {
  def apply(items: List[FungibleData]): FungibleDataGroup = {
    new FungibleDataGroup(items.sumByCurrency)
  }
}
