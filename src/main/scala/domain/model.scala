package io.softwarechain.cryptojournal
package domain

object model {
  sealed trait State
  final case object Open   extends State
  final case object Closed extends State

  sealed trait TransactionType
  final case object Unknown extends TransactionType //used as a fallback.
  final case object Buy     extends TransactionType
  final case object Sell    extends TransactionType

  final case class FungibleData(amount: BigDecimal, currency: String)

  type Fee = FungibleData
}
