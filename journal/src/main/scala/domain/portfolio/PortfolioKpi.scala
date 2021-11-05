package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ Currency, FungibleData, Mistake, Percentage, Tag, TradeCountPredicate }
import domain.portfolio.PortfolioKpi.FungibleDataOps
import domain.portfolio.model.{ DailyTradeData, DayFormat, DayPredicate }
import domain.position.{ MarketPlays, Position }
import util.{ InstantOps, MarketPlaysListOps }
import vo.filter.Count
import vo.{ PeriodDistribution, TimeInterval }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV

import java.time.format.DateTimeFormatter
import java.time.{ DayOfWeek, Duration, Month, ZoneId }

/**
 * @param marketPlays source to compute the KPIs for
 * @param interval timeInterval does not have to be an exact match with the interval of the given positions.
 * @param referenceMarketPlays marketPlays compares against referenceMarketPlays to generate performance.
 */
final case class PortfolioKpi(
  marketPlays: MarketPlays,
  interval: TimeInterval,
  referenceMarketPlays: MarketPlays = MarketPlays.empty()
) {
  lazy val netReturn: NetReturn = NetReturn(marketPlays)

  lazy val balance: AccountBalance = AccountBalance(marketPlays)

  lazy val tradeCount: Int = marketPlays.closedPositions.size

  lazy val openTradesCount: Int = marketPlays.openPositions.size

  lazy val winRate: Float = {
    winRate(marketPlays.closedPositions)
  }

  lazy val loseRate: Float = {
    if (marketPlays.closedPositions.nonEmpty) {
      1 - winRate
    } else {
      0f
    }
  }

  private def winRate(reference: List[Position]): Float =
    if (reference.nonEmpty) {
      val totalCount = reference.size
      val winCount = reference.count { position =>
        //.get is safe because win will be present on all closed positions.
        position.isWin.get
      }
      winCount / totalCount.toFloat
    } else {
      0f
    }

  /**
   * Sum all fees of positions
   */
  lazy val totalFees: FungibleData = {
    marketPlays.plays
      .map(_.totalFees())
      .collect {
        case Some(fee) => fee
      }
      .sumFungibleData()
  }

  /**
   * Uses the given time interval to derive the average trade count.
   */
  lazy val avgDailyTradeCount: Float = {
    marketPlays.closedPositions.size.toFloat / interval.dayCount.value
  }

  lazy val totalWins: Int Refined NonNegative = {
    refineV.unsafeFrom(marketPlays.closedPositions.count(_.isWin.get))
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
    marketPlays.positions.map(_.numberOfCoins).sum
  }

  lazy val avgWinningHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(marketPlays.positions.filter(p => p.isWin.isDefined && p.isWin.get)))
  }

  lazy val avgLosingHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(marketPlays.positions.filter(p => p.isLoss.isDefined && p.isLoss.get)))
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

  def coinWins(count: Count): List[(Currency, FungibleData, Percentage)] = {
    val wins = coinContributions.filter(_._2.amount > 0)
    wins.slice(0, Math.min(count, wins.size))
  }

  def coinWins(): List[(Currency, FungibleData, Percentage)] =
    coinContributions.filter(_._2.amount > 0)

  def coinLoses(count: Count): List[(Currency, FungibleData, Percentage)] = {
    val loses = coinContributions.filter(_._2.amount < 0).reverse
    loses.slice(0, Math.min(count, loses.size))
  }

  def coinLoses(): List[(Currency, FungibleData, Percentage)] =
    coinContributions.filter(_._2.amount < 0).reverse

  lazy val coinContributions: List[(Currency, FungibleData, Percentage)] = {
    marketPlays.closedPositions
      .groupBy(_.currency)
      .map {
        case (Some(currency), listOfPositions) =>
          (
            currency,
            listOfPositions.map(_.fiatReturn.getOrElse(FungibleData.zero(USDCurrency))).sumFungibleData(),
            listOfPositions.map(_.fiatReturnPercentage.getOrElse(BigDecimal(0))).sum
          )
      }
      .toList
      .sortBy(_._2)(Ordering[FungibleData].reverse)
  }

  def periodReturn(): PeriodDistribution = {
    val returnByDate = marketPlays.closedPositions
      .map(p => p.closedAt.get.toLocalDate().atStartOfDay() -> p.fiatReturn.get)
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2).sumFungibleData())
      .toList

    val returnByDay   = returnByDate.map(t => t._1.getDayOfWeek -> t._2).sumByKey()
    val returnByMonth = returnByDate.map(t => t._1.getMonth     -> t._2).sumByKey()
    val returnByYear  = returnByDate.map(t => t._1.getYear      -> t._2).sumByKey()

    PeriodDistribution(
      weekly = DayOfWeek.values().map(day => returnByDay.getOrElse(day, FungibleData.zero(USDCurrency))).toList,
      monthly = Month.values().map(month => returnByMonth.getOrElse(month, FungibleData.zero(USDCurrency))).toList,
      yearly = interval.years().map(year => year -> returnByYear.getOrElse(year, FungibleData.zero(USDCurrency))).toMap
    )
  }

  lazy val tagContribution: Map[Tag, (FungibleData, Percentage)] = {
    val journaledPositions = marketPlays.positions.collect {
      case p if p.journal.isDefined => p.journal.get -> p
    }
    val positionToFiatReturn = journaledPositions.flatMap {
      case (journal, p) =>
        journal.tags.map(s => s -> p.fiatReturn.getOrElse(FungibleData.zero(USDCurrency)))
    }.sumByKey()

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
        journal.mistakes.map(s => s -> p.fiatReturn.getOrElse(FungibleData.zero(USDCurrency)))
    }.sumByKey()

    val positionToReturnPercentage = journaledPositions.flatMap {
      case (journal, p) =>
        journal.tags.map(s => s -> p.fiatReturnPercentage.getOrElse(BigDecimal(0)))
    }.groupBy(_._1).view.mapValues(_.map(_._2).sum).toMap

    positionToFiatReturn.view.map {
      case (tag, fungibleData) =>
        tag -> (fungibleData, positionToReturnPercentage(tag))
    }.toMap
  }

  lazy val dailyContribution: Map[DayFormat, DailyTradeData] = {
    marketPlays.closedPositions
      .filter(p => interval.contains(p.closedAt.get))
      .map(p => p.closedAt.get.atBeginningOfDay() -> p)
      .groupBy(_._1)
      .map {
        case (day, list) =>
          val dailyPositions = list.map(_._2)
          val dailyTradeData = DailyTradeData(
            NetReturn(MarketPlays(dailyPositions)),
            refineV[TradeCountPredicate].unsafeFrom(dailyPositions.size)
          )
          refineV[DayPredicate].unsafeFrom(DayFormatter.format(day)) -> dailyTradeData
      }
  }

  private val USDCurrency: Currency = refineV.unsafeFrom("USD")

  private val DayFormatter = DateTimeFormatter
    .ofPattern("yyyy-MM-dd")
    .withZone(ZoneId.systemDefault())
}

object PortfolioKpi {
  implicit class FungibleDataOps[Key](items: List[(Key, FungibleData)]) {
    def sumByKey(): Map[Key, FungibleData] =
      items.groupBy(_._1).view.mapValues(_.map(_._2).sumFungibleData()).toMap
  }
}
