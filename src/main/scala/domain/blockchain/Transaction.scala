package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{Buy, CurrencyPredicate, Fee, FungibleData, Sell, TransactionType, Unknown}

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty
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
  @jsonField("value") rawValue: Double,
  @jsonField("value_quote") valueQuote: Option[Double],
  @jsonField("gas_offered") gasOffered: Int,
  @jsonField("gas_spent") gasSpent: Int,
  @jsonField("gas_price") gasPrice: Double,
  @jsonField("gas_quote") gasQuote: Double,
  @jsonField("gas_quote_rate") gasQuoteRate: Double,
  @jsonField("log_events") logEvents: List[LogEvent]
) {

  lazy val transactionType: TransactionType = logEvents.headOption.fold[TransactionType](Unknown)(event =>
    event.decoded.name match {
      case "Swap"       => Buy
      case "Withdrawal" => Sell
      case _            => Unknown
    }
  )

  lazy val instant: Instant = Instant.parse(blockSignedAt)

  lazy val hasTransactionEvents: Boolean = logEvents.nonEmpty

  //check against fromAddress.
  lazy val coin: Option[String] = transactionType match {
    case Buy =>
      logEvents.reverse
        .filter(_.decoded.name == "Transfer")
        .find(_.decoded.params.exists(param => param.name == "to" && param.value == fromAddress))
        .flatMap(_.senderContractSymbol)
    case Sell =>
      logEvents.reverse.headOption.flatMap(_.senderContractSymbol)
    case Unknown => None
  }

  //Atm we work only with WBNB so we assume it as the coin used in buy/sell operations.
  lazy val fee: Fee = FungibleData(gasSpent * gasPrice * Math.pow(10, -18), refined.refineV[CurrencyPredicate].unsafeFrom("WBNB"))

  lazy val value: Either[String, FungibleData] = transactionType match {
    case Unknown => Left("Unknown transaction type")
    case Buy =>
      (for {
        wadValue <- logEvents.last.decoded.params
                     .find(_.name == "wad")
                     .map(_.value)
                     .map(BigDecimal(_))
        decimals <- logEvents.last.senderContractDecimals
        amount   = wadValue * Math.pow(10, -decimals)
      } yield FungibleData(amount, refined.refineV[CurrencyPredicate].unsafeFrom("WBNB"))).toRight("Unable to determine value of transaction")
    case Sell =>
      (for {
        wadValue <- logEvents.head.decoded.params.find(_.name == "wad").map(_.value).map(BigDecimal(_))
        decimals <- logEvents.head.senderContractDecimals
        amount   = wadValue * Math.pow(10, -decimals)
      } yield FungibleData(amount, refined.refineV[CurrencyPredicate].unsafeFrom("WBNB"))).toRight("Unable to determine value of transaction")
  }
}

object Transaction {
  implicit val encoder: JsonCodec[Transaction] = DeriveJsonCodec.gen[Transaction]
}
