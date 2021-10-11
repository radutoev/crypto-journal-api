package io.softwarechain.cryptojournal
package config

final case class DatastoreConfig(
  address: String,
  journal: String,
  position: String,
  paginationContext: String,
  priceQuote: String,
  userWallet: String,
  wallet: String
)
