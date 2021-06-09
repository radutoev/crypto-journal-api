package io.softwarechain.cryptojournal
package infrastructure.covalent.model

import zio.json._

final case class LogEvent(
  @jsonField("sender_contract_decimals") senderContractDecimals: Int,
  @jsonField("sender_address") senderAddress: String,
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


