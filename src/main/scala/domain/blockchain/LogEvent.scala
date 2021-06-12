package io.softwarechain.cryptojournal
package domain.blockchain

import zio.json._

final case class LogEvent(
  @jsonField("sender_contract_decimals") senderContractDecimals: Option[Int],
  @jsonField("sender_name") senderName: Option[String],
  @jsonField("sender_contract_ticker_symbol") senderContractSymbol: Option[String],
  @jsonField("sender_address") senderAddress: String,
  @jsonField("sender_address_label") senderAddressLabel: Option[String],
  decoded: Decoded
)

final case class Decoded(name: String, signature: String, params: List[Param])

final case class Param(name: String, `type`: String, indexed: Boolean, decoded: Boolean, value: String)

object LogEvent {
  implicit val encoder: JsonCodec[LogEvent] = DeriveJsonCodec.gen[LogEvent]
}

object Decoded {
  implicit val encoder: JsonCodec[Decoded] = DeriveJsonCodec.gen[Decoded]
}

object Param {
  implicit val encoder: JsonCodec[Param] = DeriveJsonCodec.gen[Param]
}
