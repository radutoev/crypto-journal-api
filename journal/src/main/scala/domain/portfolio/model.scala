package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model._
import domain.position.{MarketPlays, Position}
import domain.pricequote.PriceQuotes

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.domain.portfolio.model.Hourly.HourFormatter
import io.softwarechain.cryptojournal.domain.portfolio.model.Weekday.WeekdayFormatter
import io.softwarechain.cryptojournal.util.InstantOps

import java.time.format.DateTimeFormatter

object model {
  final case class DailyTradeData(netReturn: BigDecimal, tradeCount: TradeCount)

  type NetReturnDistributionByDay = Map[DayFormat, DailyTradeData]

//  type DayPredicate = MatchesRegex[W.`"""^\d{4,5}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"""`.T]
  type DayPredicate = NonEmpty
  type DayFormat    = String Refined DayPredicate

  type BinName = String Refined NonEmpty

  sealed trait PlaysGrouping {
    def bin(position: Position): Option[BinName]
  }

  object PlaysGrouping {
    def fromString(rawValue: String): Either[String, PlaysGrouping] =
      rawValue.trim.toLowerCase match {
        case "hour"    => Right(Hourly)
        case "weekday" => Right(Weekday)
        case "month"   => Right(Monthly)
        case _         => Left(s"Unsupported value $rawValue")
      }
  }

  final case object Hourly extends PlaysGrouping {

    private val HourFormatter = DateTimeFormatter.ofPattern("HH")

    override def bin(position: Position): Option[BinName] =
      position.closedAt.map(closedAt => refineV[NonEmpty].unsafeFrom(HourFormatter.format(closedAt.toLocalDateTime())))
  }

  final case object Weekday extends PlaysGrouping {
    private val WeekdayFormatter = DateTimeFormatter.ofPattern("EEE")

    override def bin(position: Position): Option[BinName] =
      position.closedAt.map(closedAt =>
        refineV[NonEmpty].unsafeFrom(WeekdayFormatter.format(closedAt.toLocalDateTime()))
      )
  }

  final case object Monthly extends PlaysGrouping {
    private val MonthFormatter = DateTimeFormatter.ofPattern("MMM")

    override def bin(position: Position): Option[BinName] =
      position.closedAt.map(closedAt =>
        refineV[NonEmpty].unsafeFrom(MonthFormatter.format(closedAt.toLocalDateTime()))
      )
  }

  final case class BinData(
    tradeCount: TradeCount,
    numberOfCoins: BigDecimal,
    winRate: Percentage,
    netReturn: FungibleData,
//    returnPercentage: Percentage, TODO Compute percentage also.
    fees: FungibleData
  )

  object BinData {
    def apply(marketPlays: MarketPlays, quotes: PriceQuotes): BinData = {
      //TODO This is Option -> I need a way to ensure at type level the MarketPlays do have items. I don't think I can use a NonEmptyList
      // , because the empty MarketPlays case is not wrong in other places. Maybe I need a specialized type??
      val interval = marketPlays.interval.get

      val distinctValues = PlaysDistinctValues(marketPlays, interval)
      new BinData(
        tradeCount = refineV[TradeCountPredicate].unsafeFrom(distinctValues.tradeCount),
        numberOfCoins = distinctValues.numberOfCoins,
        winRate = distinctValues.winRate,
        netReturn = marketPlays.netReturn(interval, BUSD, quotes).latestValue.fungibleData,
//        returnPercentage = BigDecimal(0),
        fees = distinctValues.totalFees.getOrElse(BUSD, FungibleData.zero(BUSD))
      )
    }
  }

//  final case object Monthly extends PlaysGrouping
}
