package io.softwarechain.cryptojournal
package infrastructure.covalent.model

import zio.json.{DeriveJsonCodec, JsonCodec, jsonField}

import java.time.Instant

/**
 * @param logEvents list of events that are part of the transaction. Latest event is first item.
 */
final case class Transaction(
  @jsonField("block_signed_at") blockSignedAt: String,
  @jsonField("tx_hash") hash: String,
  @jsonField("from_address") walletAddress: String,
  successful: Boolean,
  @jsonField("log_events") logEvents: List[LogEvent]
) {

  lazy val transactionType: TransactionType = logEvents.headOption.fold[TransactionType](Unknown)(event => event.decoded.name match {
    case "Swap" => Buy
    case "Withdrawal" => Sell
    case _ => Unknown
  })

  lazy val instant: Instant = Instant.parse(blockSignedAt)
}

object Transaction {
  implicit val encoder: JsonCodec[Transaction] = DeriveJsonCodec.gen[Transaction]
}

sealed trait TransactionType
final case object Unknown extends TransactionType //used as a fallback.
final case object Buy extends TransactionType
final case object Sell extends TransactionType
