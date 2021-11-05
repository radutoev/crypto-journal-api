package io.softwarechain.cryptojournal
package domain.model

import eu.timepit.refined.refineV

object TransactionHash {
  def apply(value: String): Either[String, TransactionHash] =
    refineV[TransactionHashPredicate](value)

  def unsafeApply(value: String): TransactionHash = refineV[TransactionHashPredicate].unsafeFrom(value)
}
