package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model._
import domain.model.fungible.{FungibleDataKeyOps, FungibleDataMapOps, FungibleDataOps}
import domain.portfolio.PlaysDistinctValues.PortfolioKpiOps
import domain.position.{MarketPlays, Position}
import util.{InstantOps, ListOptionOps}
import vo.filter.Count
import vo.{PeriodDistribution, TimeInterval}

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.domain.pricequote.PriceQuotes

import java.time.format.DateTimeFormatter
import java.time.{DayOfWeek, Duration, Month, ZoneId}

/**
 * @param marketPlays source to compute the KPIs for
 * @param interval timeInterval does not have to be an exact match with the interval of the given positions.
 */
final case class PlaysDistinctValues(
  marketPlays: MarketPlays,
  interval: TimeInterval
) {
  lazy val tradeCount: Int = marketPlays.closedPositions.size

  lazy val openTradesCount: Int = marketPlays.openPositions.size

  lazy val winRate: Float = {
    val wins = marketPlays.wins
    if (wins.nonEmpty) {
      val winCount = wins.size
      val total    = marketPlays.closedPositions.size
      winCount.toFloat / total.toFloat
    } else {
      0f
    }
  }

  lazy val loseRate: Float = {
    if (marketPlays.closedPositions.nonEmpty) {
      1 - winRate
    } else {
      0f
    }
  }

  /**
   * Sum all fees of positions
   */
  lazy val totalFees: Map[Currency, FungibleData] = {
    marketPlays.plays
      .map(_.fees())
      .sumByCurrency
  }

  /**
   * Uses the given time interval to derive the average trade count.
   */
  lazy val avgDailyTradeCount: Float = {
    marketPlays.closedPositions.size.toFloat / interval.dayCount.value
  }

  lazy val totalWins: Int Refined NonNegative = {
    refineV.unsafeFrom(marketPlays.wins.size)
  }

  lazy val totalLoses: Int Refined NonNegative = {
    refineV.unsafeFrom(marketPlays.closedPositions.size - totalWins.value)
  }

  lazy val maxConsecutiveWins: Int Refined NonNegative = {
    var max    = 0
    var streak = 0

    def onReset(): Unit = {
      max = Math.max(streak, max)
      streak = 0
    }

    marketPlays.closedPositions.foreach { p =>
      p.isWin match {
        case Some(value) =>
          if (value) {
            streak = streak + 1
          } else {
            onReset()
          }
        case None =>
      }
    }

    refineV.unsafeFrom(max)
  }

  lazy val maxConsecutiveLoses: Int Refined NonNegative = {
    var max    = 0
    var streak = 0

    def onReset(): Unit = {
      max = Math.max(streak, max)
      streak = 0
    }

    marketPlays.closedPositions.foreach { p =>
      p.isWin match {
        case Some(value) =>
          if (value) {
            onReset()
          } else {
            streak = streak + 1
          }
        case None =>
      }
    }

    refineV.unsafeFrom(max)
  }

  lazy val totalCoins: BigDecimal = {
    marketPlays.positions.map(_.totalCoins.amount).sum
  }

  lazy val numberOfCoins: BigDecimal = {
    marketPlays.positions.map(_.numberOfCoins).sum
  }

  lazy val avgWinningHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(marketPlays.wins))
  }

  lazy val avgLosingHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(marketPlays.loses))
  }

  def avgHoldTime(items: List[Position]): Long = {
    val list = items.map(_.holdTime).collect {
      case Some(value) => value
    }
    if (list.nonEmpty) {
      list.sum / list.size
    } else {
      0L
    }
  }

  lazy val biggestWin: Option[FungibleData] = {
    marketPlays.closedPositions.collect {
      case p: Position if p.fiatReturn.isDefined => p.fiatReturn.get
    }.maxOption
  }

  lazy val biggestLoss: Option[FungibleData] = {
    marketPlays.closedPositions.collect {
      case p: Position if p.fiatReturn.isDefined => p.fiatReturn.get
    }.minOption
  }

  def coinWins(count: Count): List[(Currency, FungibleData, Percentage)] =
    coinWins.slice(0, Math.min(count, coinWins.size))

  lazy val coinWins: List[(Currency, FungibleData, Percentage)] = {
    marketPlays.wins.asCoinContributions
  }

  def coinLoses(count: Count): List[(Currency, FungibleData, Percentage)] =
    coinLoses.slice(0, Math.min(count, coinLoses.size))

  lazy val coinLoses: List[(Currency, FungibleData, Percentage)] =
    marketPlays.loses.asCoinContributions.reverse

  def periodReturn(): PeriodDistribution = {
    val returnByDate = marketPlays.closedPositions
      .map(p => p.closedAt.get.toLocalDate().atStartOfDay() -> p.fiatReturn.get)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sumByCurrency.getOrElse(BUSD, FungibleData.zero(BUSD)))
      .toList

    val returnByDay   = returnByDate.map(t => t._1.getDayOfWeek -> t._2).sumByKey()
    val returnByMonth = returnByDate.map(t => t._1.getMonth     -> t._2).sumByKey()
    val returnByYear  = returnByDate.map(t => t._1.getYear      -> t._2).sumByKey()

    PeriodDistribution(
      weekly = DayOfWeek
        .values()
        .map(day => returnByDay.get(day).flatMap(_.get(BUSD)).getOrElse(FungibleData.zero(BUSD)))
        .toList,
      monthly = Month
        .values()
        .map(month => returnByMonth.get(month).flatMap(_.get(BUSD)).getOrElse(FungibleData.zero(BUSD)))
        .toList,
      yearly = interval
        .years()
        .map(year => year -> returnByYear.get(year).flatMap(_.get(BUSD)).getOrElse(FungibleData.zero(BUSD)))
        .toMap
    )
  }

  lazy val tagContribution: Map[Tag, (FungibleData, Percentage)] = {
    val journaledPositions = marketPlays.positions.collect {
      case p if p.journal.isDefined => p.journal.get -> p
    }
    val positionToFiatReturn = journaledPositions.flatMap {
      case (journal, p) =>
        journal.tags.map(s => s -> p.fiatReturn.getOrElse(FungibleData.zero(BUSD)))
    }.sumByKey().map {
      case (tag, currencySum) =>
        tag -> currencySum.getOrElse(BUSD, FungibleData.zero(BUSD))
    }

    val positionToReturnPercentage = journaledPositions.flatMap {
      case (journal, p) =>
        journal.tags.map(s => s -> p.fiatReturnPercentage.getOrElse(BigDecimal(0)))
    }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

    positionToFiatReturn.view.map {
      case (tag, fungibleData) =>
        tag -> (fungibleData, positionToReturnPercentage(tag))
    }.toMap
  }

  lazy val mistakeContribution: Map[Mistake, (FungibleData, Percentage)] = {
    val journaledPositions = marketPlays.positions.collect {
      case p if p.journal.isDefined => p.journal.get -> p
    }

    val positionToFiatReturn = journaledPositions.flatMap {
      case (journal, p) =>
        journal.mistakes.map(s => s -> p.fiatReturn.getOrElse(FungibleData.zero(BUSD)))
    }.sumByKey().map {
      case (mistake, currencySum) =>
        mistake -> currencySum.getOrElse(BUSD, FungibleData.zero(BUSD))
    }

    val positionToReturnPercentage = journaledPositions.flatMap {
      case (journal, p) =>
        journal.tags.map(s => s -> p.fiatReturnPercentage.getOrElse(BigDecimal(0)))
    }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

    positionToFiatReturn.view.map {
      case (tag, fungibleData) =>
        tag -> (fungibleData, positionToReturnPercentage(tag))
    }.toMap
  }

  //TODO Re-implement this
//  lazy val dailyContribution: Map[DayFormat, DailyTradeData] = {
//    marketPlays.closedPositions
//      .filter(p => interval.contains(p.closedAt.get))
//      .map(p => p.closedAt.get.atBeginningOfDay() -> p)
//      .groupBy(_._1)
//      .map {
//        case (day, list) =>
//          val dailyPositions = list.map(_._2)
//          val dailyTradeData = DailyTradeData(
//            NetReturn(MarketPlays(dailyPositions)),
//            refineV[TradeCountPredicate].unsafeFrom(dailyPositions.size)
//          )
//          refineV[DayPredicate].unsafeFrom(DayFormatter.format(day)) -> dailyTradeData
//      }
//  }
}

object PlaysDistinctValues {
  private[portfolio] val DayFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd")
    .withZone(ZoneId.systemDefault())

  implicit class PortfolioKpiOps(positions: List[Position]) {
    lazy val asCoinContributions: List[(Currency, Fee, Percentage)] = {
      positions
        .groupBy(_.currency)
        .map {
          case (Some(currency), listOfPositions) =>
            (
              currency,
              listOfPositions.map(_.fiatReturn).values.sumByCurrency.getOrElse(BUSD, FungibleData.zero(BUSD)),
              listOfPositions.map(_.fiatReturnPercentage).values.sum
            )
        }
        .toList
        .sortBy(_._2)(Ordering[FungibleData].reverse)
    }
  }
}
