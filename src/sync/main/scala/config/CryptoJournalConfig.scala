package io.softwarechain.cryptojournal
package config

import zio.config.magnolia.DeriveConfigDescriptor
import zio.config.refined._

object CryptoJournalConfig {
  val descriptor = DeriveConfigDescriptor.descriptor[CryptoJournalConfig]
}

final case class CryptoJournalConfig(
  covalent: CovalentConfig
)
