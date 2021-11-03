package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{
//  AirDrop,
//  Approval,
//  Buy,
//  Claim,
//  Contribute,
  Currency,
  Fee,
  FungibleData,
//  Sell,
  TransactionHash,
  TransactionType,
//  TransferIn,
//  TransferOut,
  Unknown
}

import java.time.Instant

/**
 * @param logEvents list of events that are part of the transaction. Latest event is first item.
 * TODO Refine params.
 */
final case class Transaction(
  blockSignedAt: String,
  hash: TransactionHash,
  successful: Boolean,
  fromAddress: String,
  fromAddressLabel: Option[String],
  toAddress: String,
  toAddressLabel: Option[String],
  rawValue: String,
  valueQuote: Option[Double],
  gasOffered: Int,
  gasSpent: Int,
  gasPrice: Double,
  gasQuote: Double,
  gasQuoteRate: Double,
  logEvents: List[LogEvent]
) {

  lazy val instant: Instant = Instant.parse(blockSignedAt)

  lazy val hasTransactionEvents: Boolean = logEvents.nonEmpty
}
