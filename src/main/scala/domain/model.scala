package io.softwarechain.cryptojournal
package domain

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{ NonEmpty, Size }
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import eu.timepit.refined.types.string.NonEmptyString

object model {
  type CurrencyPredicate = NonEmpty
  type Currency          = String Refined CurrencyPredicate

  type NumberOfDaysPredicate = NonNegative
  type NumberOfDays = Int Refined NumberOfDaysPredicate

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

  final case class FungibleData(amount: BigDecimal, currency: Currency) {
    def add(value: BigDecimal): FungibleData = copy(amount = amount + value)

    def subtract(value: BigDecimal): FungibleData = copy(amount = amount - value)

    def negate(): FungibleData = copy(amount = -amount)
  }

  object FungibleData {
    def zero(currency: Currency): FungibleData = new FungibleData(0, currency)

    def apply(amount: BigDecimal, currency: Currency): FungibleData = new FungibleData(amount, currency)

    //`Ordering[A]` is not contravariant => the declaration
    // must be type-parametrized for implicit ordering of subclasses of `FungibleData`.
    implicit def orderingByAmount[A <: FungibleData]: Ordering[A] = Ordering.by(_.amount)
  }

  type Fee = FungibleData

  type TransactionHashPredicate = NonEmpty
  type TransactionHash          = String Refined TransactionHashPredicate

  object TransactionHash {
    def apply(value: String): Either[String, TransactionHash] =
      refineV[NonEmpty](value)
  }

  type WalletAddressPredicate = And[Size[Equal[42]], MatchesRegex["0x[a-zA-Z0-9]{40}"]]
  type WalletAddress          = String Refined WalletAddressPredicate

  type UserId = NonEmptyString

  type TradeCountPredicate = NonNegative
  type TradeCount          = Int Refined TradeCountPredicate

  final case class UserWallet(userId: UserId, address: WalletAddress)

  implicit class FungibleDataOps(list: List[FungibleData]) {
    def sumFungibleData(): FungibleData =
      list.foldLeft(FungibleData(BigDecimal(0), refineV[NonEmpty]("USD").right.get)) { (acc, value) =>
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
