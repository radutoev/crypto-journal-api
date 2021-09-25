package io.softwarechain.cryptojournal

import eu.timepit.refined.types.numeric.PosInt
import zio.config.magnolia.DeriveConfigDescriptor
import zio.config.refined._

object CryptoJournalConfig {
  val descriptor = DeriveConfigDescriptor.descriptor[CryptoJournalConfig]
}

final case class CryptoJournalConfig(
  coinApi: CoinApiConfig,
  covalent: CovalentConfig,
  demoAccount: DemoAccountConfig,
  datastore: DatastoreConfig
)

final case class CovalentConfig(baseUrl: String, key: String)

final case class CoinApiConfig(baseUrl: String, key: String)

final case class DemoAccountConfig(maxPositions: PosInt)

final case class DatastoreConfig(
  address: String,
  checkpoint: String,
  journal: String,
  position: String,
  priceQuote: String,
  wallet: String
)
