package io.softwarechain.cryptojournal

import zio.config.magnolia.DeriveConfigDescriptor

object CryptoJournalConfig {
  val descriptor = DeriveConfigDescriptor.descriptor[CryptoJournalConfig]
}

final case class CryptoJournalConfig(covalent: CovalentConfig)

final case class CovalentConfig(baseUrl: String, key: String, demoTxCount: Int = 30)
