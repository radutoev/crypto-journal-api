package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model._
import domain.position.{MarketPlays, Position}
import domain.pricequote.PriceQuotes
import util.{InstantOps, ListOptionOps}

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.infrastructure.api.portfolio.CoinToFungiblePair

import java.time.format.DateTimeFormatter

object model {
  final case class DailyTradeData(netReturn: BigDecimal, tradeCount: TradeCount)

  type NetReturnDistributionByDay = Map[DayFormat, DailyTradeData]

//  type DayPredicate = MatchesRegex[W.`"""^\d{4,5}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"""`.T]
  type DayPredicate = NonEmpty
  type DayFormat    = String Refined DayPredicate

  type BinNamePredicate = NonEmpty
  type BinName          = String Refined BinNamePredicate

  sealed trait PlaysGrouping {
    def bin(position: Position): Option[Set[BinName]]
  }

  object PlaysGrouping {
    def fromString(rawValue: String): Either[String, PlaysGrouping] =
      rawValue.trim.toLowerCase match {
        case "hour"    => Right(Hourly)
        case "weekday" => Right(Weekday)
        case "month"   => Right(Monthly)
        case "year"    => Right(Yearly)
        case "tag"     => Right(Tag)
        case "mistake" => Right(Mistake)
        case _         => Left(s"Unsupported value $rawValue")
      }
  }

  final case object Hourly extends PlaysGrouping {

    private val HourFormatter = DateTimeFormatter.ofPattern("HH")

    override def bin(position: Position): Option[Set[BinName]] =
      position.closedAt
        .map(closedAt => Set(refineV[BinNamePredicate].unsafeFrom(HourFormatter.format(closedAt.toLocalDateTime()))))
  }

  final case object Weekday extends PlaysGrouping {
    private val WeekdayFormatter = DateTimeFormatter.ofPattern("EEE")

    override def bin(position: Position): Option[Set[BinName]] =
      position.closedAt.map(closedAt =>
        Set(refineV[BinNamePredicate].unsafeFrom(WeekdayFormatter.format(closedAt.toLocalDateTime())))
      )
  }

  final case object Monthly extends PlaysGrouping {
    private val MonthFormatter = DateTimeFormatter.ofPattern("MMM")

    override def bin(position: Position): Option[Set[BinName]] =
      position.closedAt.map(closedAt =>
        Set(refineV[BinNamePredicate].unsafeFrom(MonthFormatter.format(closedAt.toLocalDateTime())))
      )
  }

  final case object Yearly extends PlaysGrouping {
    private val YearFormatter = DateTimeFormatter.ofPattern("yyyy")

    override def bin(position: Position): Option[Set[BinName]] =
      position.closedAt.map(closedAt =>
        Set(refineV[BinNamePredicate].unsafeFrom(YearFormatter.format(closedAt.toLocalDateTime())))
      )
  }

  final case object Tag extends PlaysGrouping {
    override def bin(position: Position): Option[Set[BinName]] =
      position.journal.map(
        _.tags
          .map(_.value)
          .map(tag => refineV[BinNamePredicate].unsafeFrom(tag))
          .toSet
      )
  }

  final case object Mistake extends PlaysGrouping {
    override def bin(position: Position): Option[Set[BinName]] =
      position.journal.map(_.mistakes.map(_.value).map(mistake => refineV[BinNamePredicate].unsafeFrom(mistake)).toSet)
  }

  final case class BinData(
    tradeCount: TradeCount,
    numberOfCoins: BigDecimal,
    winRate: Percentage,
    netReturn: FungibleData,
    returnPercentage: Percentage,
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
        returnPercentage = marketPlays.closedPositions.map(_.fiatReturnPercentage).values.sum,
        fees = distinctValues.totalFees.getOrElse(BUSD, FungibleData.zero(BUSD))
      )
    }
  }

  final case class BinDataAndSources(binData: BinData, source: List[Position], withSourceData: Boolean = false)

  final case class CoinToFungiblePair(currency: Currency, fungibleData: FungibleData, percentage: BigDecimal)

  final case class TradeSummary(wins: List[CoinToFungiblePair], loses: List[CoinToFungiblePair])

  object TradeSummary {
    def empty(): TradeSummary = TradeSummary(List.empty, List.empty)
  }
}
