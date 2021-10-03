package io.softwarechain.cryptojournal
package domain.blockchain

final case class LogEvent(
  senderContractDecimals: Option[Int],
  senderName: Option[String],
  senderContractSymbol: Option[String],
  senderAddress: String,
  senderAddressLabel: Option[String],
  decoded: Decoded
)

final case class Decoded(name: String, signature: String, params: List[Param])

final case class Param(name: String, `type`: String, indexed: Boolean, decoded: Boolean, value: String)
