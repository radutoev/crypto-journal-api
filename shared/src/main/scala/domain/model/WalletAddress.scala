package io.softwarechain.cryptojournal
package domain.model

import eu.timepit.refined.refineV

object WalletAddress {
  def unsafeFrom(value: String): WalletAddress = refineV[WalletAddressPredicate].unsafeFrom(value)
}
