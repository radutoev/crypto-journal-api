package io.softwarechain.cryptojournal
package domain

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.Size
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString

object model {
  sealed trait State
  final case object Open   extends State
  final case object Closed extends State

  object State {
    def apply(value: String): Either[String, State] =
      value.trim.toLowerCase match {
        case "open"   => Right(Open)
        case "closed" => Right(Closed)
        case _        => Left(s"Invalid state representation: $value")
      }
  }

  sealed trait TransactionType
  final case object Unknown extends TransactionType //used as a fallback.
  final case object Buy     extends TransactionType
  final case object Sell    extends TransactionType

  object TransactionType {
    def apply(value: String): TransactionType =
      value.trim.toLowerCase match {
        case "buy"  => Buy
        case "sell" => Sell
        case _      => Unknown
      }
  }

  final case class FungibleData(amount: BigDecimal, currency: String) {
    def add(value: BigDecimal): FungibleData = copy(amount = amount + value)
  }

  type Fee = FungibleData

  type WalletAddressPredicate = And[Size[Equal[42]], MatchesRegex["0x[a-z0-9]{40}"]]
  type WalletAddress          = String Refined WalletAddressPredicate

  type UserId = NonEmptyString

  implicit class FungibleDataOps(list: List[FungibleData]) {
    def sumFungibleData(): FungibleData = list.foldLeft(FungibleData(BigDecimal(0), "USD")) { (acc, value) =>
      acc.add(value.amount)
    }
  }

  implicit class OptionalFungibleDataOps(list: List[Option[FungibleData]]) {
    def sumFungibleData(): FungibleData =
      list.collect {
        case Some(value) => value
      }.sumFungibleData()
  }
}
