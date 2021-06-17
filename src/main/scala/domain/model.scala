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

  final case class FungibleData(amount: BigDecimal, currency: String) {
    def add(value: BigDecimal): FungibleData = copy(amount = amount + value)
  }

  type Fee = FungibleData

  implicit class FungibleDataOps(list: List[FungibleData]) {
    def sumFungibleData(): FungibleData = list.foldLeft(FungibleData(BigDecimal(0), "USD")) { (acc, value) =>
      acc.add(value.amount)
    }
  }

  implicit class OptionalFungibleDataOps(list: List[Option[FungibleData]]) {
    def sumFungibleData(): FungibleData = list.collect  {
      case Some(value) => value
    }.sumFungibleData()
  }
}
