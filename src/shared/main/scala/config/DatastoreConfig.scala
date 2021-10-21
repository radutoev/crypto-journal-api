package io.softwarechain.cryptojournal
package config

final case class DatastoreConfig(
  address: String,
  journal: String,
  marketPlay: String,
  paginationContext: String,
  priceQuote: String,
  userWallet: String,
  wallet: String
)
