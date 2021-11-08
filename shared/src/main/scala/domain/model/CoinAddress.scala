package io.softwarechain.cryptojournal
package domain.model

import eu.timepit.refined.refineV

object CoinAddress {
  def unsafeFrom(value: String): CoinAddress = refineV[CoinAddressPredicate].unsafeFrom(value)
}
