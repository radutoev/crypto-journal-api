package io.softwarechain.cryptojournal
package domain.model

import domain.model.FungibleData.{Bigger, ComparisonResult, DifferentCurrencies, FungibleDataError, Lower}

import currencyops.CurrencyOps
import util.{ListOptionOps, math}

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

object fungible {
  implicit class FungibleDataOps(list: List[FungibleData]) {
    lazy val sumByCurrency: Map[Currency, FungibleData] =
      list.groupBy(_.currency).map { case (currency, values) =>
        currency -> values.foldLeft(FungibleData(BigDecimal(0), currency))((acc, value) => acc.add(value.amount))
      }

    def sumOfCurrency(currency: Currency): FungibleData = {
      list.filter(_.currency == currency).foldLeft(FungibleData(BigDecimal(0), currency))((acc, value) => acc.add(value.amount))
    }
  }

  implicit class OptionalFungibleDataOps(list: List[Option[FungibleData]]) {
    lazy val sumByCurrency: Map[Currency, FungibleData]  = {
      list.values.sumByCurrency
    }
  }

  implicit class FungibleDataKeyOps[Key](items: List[(Key, FungibleData)]) {
    def sumByKey(): Map[Key, Map[Currency, FungibleData]] =
      items.groupBy(_._1).view.mapValues(_.map(_._2).sumByCurrency).toMap
  }
}
