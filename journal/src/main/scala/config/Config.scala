package io.softwarechain.cryptojournal
package config

import eu.timepit.refined.types.numeric.PosInt
import zio.config.magnolia.DeriveConfigDescriptor
import zio.config.refined._

object CryptoJournalConfig {
  val descriptor: zio.config.ConfigDescriptor[CryptoJournalConfig] =
    DeriveConfigDescriptor.descriptor[CryptoJournalConfig]
}

final case class CryptoJournalConfig(
  bitquery: BitQueryConfig,
  covalent: CovalentConfig,
  demoAccount: DemoAccountConfig,
  datastore: DatastoreConfig,
  sync: SyncConfig
)

final case class DemoAccountConfig(maxPositions: PosInt)

final case class SyncConfig(url: String)
