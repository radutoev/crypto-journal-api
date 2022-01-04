package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData.{Bigger, Equal, Lower}
import domain.portfolio.model.{Decrease, Increase, Performance}
import domain.portfolio.model.Performance.NoChangeInPerformance
import domain.position.FungibleDataTimePoint

import zio.prelude.NonEmptyList

final case class Trend (items: NonEmptyList[FungibleDataTimePoint]) {
  lazy val latestValue: FungibleDataTimePoint = items.last

  def performance(other: Trend): Performance = {
    val data = latestValue.fungibleData
    val otherData = other.latestValue.fungibleData
    data.compare(other.latestValue.fungibleData) match {
      case Left(_) => NoChangeInPerformance
      case Right(comparisonResult) =>
        comparisonResult match {
          case Equal => NoChangeInPerformance
          case Bigger =>
            Performance(
              absolute = data.difference(otherData).getOrElse(BigDecimal(0)),
              percentage = data.percentageDifference(otherData).getOrElse(BigDecimal(0)),
              trend = Increase
            )
          case Lower =>
            Performance(
              absolute = data.difference(otherData).getOrElse(BigDecimal(0)),
              percentage = data.percentageDifference(otherData).getOrElse(BigDecimal(0)),
              trend = Decrease
            )
        }
    }
  }
}

object Trend {
  def apply(source: List[FungibleDataTimePoint]): Either[String, Trend] = {
    if(source.nonEmpty) {
      Right(new Trend(NonEmptyList.fromIterable(source.head, source.tail)))
    } else {
      Left("Empty trend source")
    }
  }
}
