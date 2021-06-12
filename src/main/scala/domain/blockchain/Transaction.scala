package io.softwarechain.cryptojournal
package domain.blockchain

import zio.json.{DeriveJsonCodec, JsonCodec, jsonField}

import java.time.Instant

/**
 * @param logEvents list of events that are part of the transaction. Latest event is first item.
 */
final case class Transaction(
    @jsonField("block_signed_at") blockSignedAt: String,
    @jsonField("tx_hash") hash: String,
    successful: Boolean,
    @jsonField("from_address") fromAddress: String,
    @jsonField("from_address_label") fromAddressLabel: Option[String],
    @jsonField("to_address") toAddress: String,
    @jsonField("to_address_label") toAddressLabel: Option[String],
    value: Double,
    @jsonField("value_quote") valueQuote: Option[Double],
    @jsonField("gas_offered") gasOffered: Int,
    @jsonField("gas_spent") gasSpent: Int,
    @jsonField("gas_price") gasPrice: Option[Double],
    @jsonField("gas_quote") gasQuote: Double,
    @jsonField("gas_quote_rate") gasQuoteRate: Double,
    @jsonField("log_events") logEvents: List[LogEvent]
) {

  lazy val transactionType: TransactionType = logEvents.headOption.fold[TransactionType](Unknown)(event => event.decoded.name match {
    case "Swap" => Buy
    case "Withdrawal" => Sell
    case _ => Unknown
  })

  lazy val instant: Instant = Instant.parse(blockSignedAt)

  lazy val hasTransactionEvents: Boolean = logEvents.nonEmpty

  //check against fromAddress.
  lazy val coin: Option[String] = transactionType match {
    case Buy =>
      logEvents
        .reverse
        .filter(_.decoded.name == "Transfer")
        .find(_.decoded.params.exists(param => param.name == "to" && param.value == fromAddress))
        .flatMap(_.senderContractSymbol)
    case Sell =>
       logEvents.reverse.headOption.flatMap(_.senderContractSymbol)
    case Unknown => None
  }
}

object Transaction {
  implicit val encoder: JsonCodec[Transaction] = DeriveJsonCodec.gen[Transaction]
}

sealed trait TransactionType
final case object Unknown extends TransactionType //used as a fallback.
final case object Buy extends TransactionType
final case object Sell extends TransactionType
