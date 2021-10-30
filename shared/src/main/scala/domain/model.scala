package io.softwarechain.cryptojournal
package domain

import domain.model.FungibleData.{ Bigger, ComparisonResult, DifferentCurrencies, FungibleDataError, Lower }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{ NonEmpty, Size }
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import eu.timepit.refined.string.MatchesRegex
import util.math

object model {
  type CurrencyPredicate = NonEmpty
  type Currency          = String Refined CurrencyPredicate

  object Currency {
    def apply(str: String): Either[String, Currency] = refineV[CurrencyPredicate](str)

    def unsafeFrom(str: String): Refined[String, CurrencyPredicate] = refineV[CurrencyPredicate].unsafeFrom(str)
  }

  implicit class CurrencyOps(currency: Currency) {
    def sameCurrency(other: Currency): Boolean = currency.value == other.value
  }

  type NumberOfDaysPredicate = NonNegative
  type NumberOfDays          = Int Refined NumberOfDaysPredicate

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
  final case object Unknown     extends TransactionType //used as a fallback.
//  final case object AirDrop     extends TransactionType
//  final case object Approval    extends TransactionType
//  final case object Buy         extends TransactionType
//  final case object Sell        extends TransactionType
//  final case object TransferIn  extends TransactionType
//  final case object TransferOut extends TransactionType
//  final case object Contribute  extends TransactionType
//  final case object Claim       extends TransactionType

  object TransactionType {
    def apply(value: String): TransactionType =
      value.trim.toLowerCase match {
//        case "buy"         => Buy
//        case "sell"        => Sell
//        case "transferin"  => TransferIn
//        case "transferout" => TransferOut
//        case "contribute"  => Contribute
//        case "claim"       => Claim
        case _             => Unknown
      }
  }

  final case class FungibleData(amount: BigDecimal, currency: Currency) {
    def add(value: BigDecimal): FungibleData = copy(amount = amount + value)

    def subtract(value: BigDecimal): FungibleData = copy(amount = amount - value)

    def negate(): FungibleData = copy(amount = -amount)

    def divide(denominator: Int): FungibleData = copy(amount = amount / denominator)

    def compare(other: FungibleData): Either[FungibleDataError, ComparisonResult] =
      fnOnFungibleData(
        (f1, f2) =>
          f1.amount.compare(f2.amount) match {
            case -1 => Lower
            case 0  => FungibleData.Equal
            case 1  => Bigger
          },
        other
      )

    def difference(other: FungibleData): Either[FungibleDataError, BigDecimal] =
      fnOnFungibleData((f1, f2) => f1.amount - f2.amount, other)

    /**
     * Formula used: x1,x2 where x2 after x1, then the percentage difference is:
     * ((x2 - x1) / x1) * 100.
     */
    def percentageDifference(other: FungibleData): Either[FungibleDataError, BigDecimal] =
      fnOnFungibleData((f, fPrev) => math.percentageDiff(f.amount, fPrev.amount), other)

    private def fnOnFungibleData[T](
      fn: (FungibleData, FungibleData) => T,
      other: FungibleData
    ): Either[FungibleDataError, T] =
      if (other.currency.sameCurrency(currency)) {
        Right {
          fn(this, other)
        }
      } else {
        Left(DifferentCurrencies)
      }
  }

  object FungibleData {
    def zero(currency: Currency): FungibleData = new FungibleData(0, currency)

    def apply(amount: BigDecimal, currency: Currency): FungibleData = new FungibleData(amount, currency)

    //`Ordering[A]` is not contravariant => the declaration
    // must be type-parametrized for implicit ordering of subclasses of `FungibleData`.
    implicit def orderingByAmount[A <: FungibleData]: Ordering[A] = Ordering.by(_.amount)

    sealed trait ComparisonResult
    final case object Equal  extends ComparisonResult
    final case object Bigger extends ComparisonResult
    final case object Lower  extends ComparisonResult

    sealed trait FungibleDataError
    final case object DifferentCurrencies extends FungibleDataError
  }

  type Fee = FungibleData

  type TransactionHashPredicate = NonEmpty
  type TransactionHash          = String Refined TransactionHashPredicate

  object TransactionHash {
    def apply(value: String): Either[String, TransactionHash] =
      refineV[TransactionHashPredicate](value)

    def unsafeApply(value: String): TransactionHash = refineV[TransactionHashPredicate].unsafeFrom(value)
  }

  type WalletAddressPredicate = And[Size[Equal[42]], MatchesRegex["0x[a-zA-Z0-9]{40}"]]
  type WalletAddress          = String Refined WalletAddressPredicate

  object WalletAddress {
    def unsafeFrom(value: String): WalletAddress = refineV[WalletAddressPredicate].unsafeFrom(value)
  }

  type UserIdPredicate = NonEmpty
  type UserId          = String Refined UserIdPredicate

  type ContextIdPredicate = NonEmpty
  type ContextId          = String Refined ContextIdPredicate

  type TradeCountPredicate = NonNegative
  type TradeCount          = Int Refined TradeCountPredicate

  type TagPredicate = NonEmpty
  type Tag          = String Refined TagPredicate

  type MistakePredicate = NonEmpty
  type Mistake          = String Refined MistakePredicate

  type Percentage = BigDecimal

  type PlayIdPredicate = NonEmpty
  type PlayId          = String Refined PlayIdPredicate

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
