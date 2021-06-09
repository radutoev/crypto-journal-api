package io.softwarechain.cryptojournal
package infrastructure.covalent

import zio.json.{DeriveJsonCodec, DeriveJsonEncoder, JsonCodec, jsonField}

/**
 * I don't set all properties from json payload.
 */
final case class Transaction(
  @jsonField("tx_hash") hash: String,
  successful: Boolean,
  @jsonField("log_events") logEvents: List[LogEvent]
)

object Transaction {
  implicit val encoder: JsonCodec[Transaction] = DeriveJsonCodec.gen[Transaction]
}
