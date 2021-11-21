package io.softwarechain.cryptojournal
package config

import zio.config.magnolia.DeriveConfigDescriptor

object CryptoJournalConfig {
  val descriptor = DeriveConfigDescriptor.descriptor[CryptoJournalConfig]
}

final case class CryptoJournalConfig(
  datastore: DatastoreConfig
)
