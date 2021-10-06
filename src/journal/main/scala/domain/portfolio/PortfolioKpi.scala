package io.softwarechain.cryptojournal
package domain.portfolio

import PortfolioKpi.FungibleDataOps
import domain.model.{ Currency, FungibleData, Mistake, Setup }
import domain.position.{ Position, Positions }
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.refineV
import util.InstantOps
import vo.{ PeriodDistribution, TimeInterval }

import java.time.{ DayOfWeek, Duration, Instant, Month }

final class PortfolioKpi(positions: Positions, interval: TimeInterval) {
  lazy val tradeCount: Int = positions.closedPositions.size

  lazy val openTradesCount: Int = positions.openPositions.size

  lazy val winRate: Float = {
    winRate(positions.closedPositions)
  }

  lazy val loseRate: Float = {
    if (positions.closedPositions.nonEmpty) {
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
        position.isWin().get
      }
      winCount / totalCount.toFloat
    } else {
      0f
    }

  lazy val netReturn: FungibleData = {
    positions.closedPositions.map(_.fiatReturn()).sumFungibleData()
  }

  /**
   * Account balance derived from provided positions.
   */
  lazy val balance: FungibleData = {
    positions.items.map(_.fiatValue()).sumFungibleData()
  }

  lazy val balanceTrend: List[FungibleData] = {
    trend(_.fiatValue())
  }

  lazy val netReturnTrend: List[FungibleData] = {
    trend(_.fiatReturn())
  }

  private def trend(of: Position => Option[FungibleData]): List[FungibleData] =
    positions.items.headOption.map(_.openedAt).fold[List[FungibleData]](List.empty) { openedAt =>
      val interval = TimeInterval(openedAt.atBeginningOfDay(), Instant.now()) //should be an implicit
      interval.days().map(day => positions.filter(TimeInterval(interval.start, day)).items.map(of).sumFungibleData())
    }

  /**
   * Sum all fees of positions
   */
  lazy val totalFees: FungibleData = {
    positions.items
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
    positions.closedPositions.size.toFloat / interval.dayCount.value
  }

  lazy val totalWins: Int Refined NonNegative = {
    refineV.unsafeFrom(positions.closedPositions.count(_.isWin().get))
  }

  lazy val totalLoses: Int Refined NonNegative = {
    refineV.unsafeFrom(positions.closedPositions.size - totalWins.value)
  }

  lazy val maxConsecutiveWins: Int Refined NonNegative = {
    var max    = 0
    var streak = 0

    def onReset(): Unit = {
      max = Math.max(streak, max)
      streak = 0
    }

    positions.closedPositions.foreach { p =>
      p.isWin() match {
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

    positions.closedPositions.foreach { p =>
      p.isWin() match {
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
    positions.items.map(_.numberOfCoins()).sum
  }

  lazy val avgWinningHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(positions.items.filter(p => p.isWin().isDefined && p.isWin().get)))
  }

  lazy val avgLosingHoldTime: Duration = {
    Duration.ofSeconds(avgHoldTime(positions.items.filter(p => p.isLoss().isDefined && p.isLoss().get)))
  }

  def avgHoldTime(items: List[Position]): Long = {
    val list = items.map(_.holdTime()).collect {
      case Some(value) => value
    }
    if (list.nonEmpty) {
      list.sum / list.size
    } else {
      0L
    }
  }

  lazy val biggestWin: Option[FungibleData] = {
    positions.closedPositions.collect {
      case p: Position if p.fiatReturn().isDefined => p.fiatReturn().get
    }.maxOption
  }

  lazy val biggestLoss: Option[FungibleData] = {
    positions.closedPositions.collect {
      case p: Position if p.fiatReturn().isDefined => p.fiatReturn().get
    }.minOption
  }

  lazy val coinWins: List[(Currency, FungibleData)] = {
    val wins = coinContributions.filter(_._2.amount > 0)
    wins.slice(0, Math.min(8, wins.size))
  }

  lazy val coinLoses: List[(Currency, FungibleData)] = {
    val loses = coinContributions.filter(_._2.amount < 0).reverse
    loses.slice(0, Math.min(8, loses.size))
  }

  lazy val coinContributions: List[(Currency, FungibleData)] = {
    positions.closedPositions
      .groupBy(_.currency)
      .map {
        case (currency, listOfPositions) =>
          currency -> listOfPositions.map(_.fiatReturn().getOrElse(FungibleData.zero(USDCurrency))).sumFungibleData()
      }
      .toList
      .sortBy(_._2)(Ordering[FungibleData].reverse)
  }

  def periodReturn(): PeriodDistribution = {
    val returnByDate = positions.closedPositions
      .map(p => p.closedAt().get.toLocalDate().atStartOfDay() -> p.fiatReturn().get)
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

  lazy val setupContributions: Map[Setup, FungibleData] =
    positions.items.collect {
      case p if p.journal.isDefined => p.journal.get -> p
    }.flatMap {
      case (journal, p) =>
        journal.setups.map(s => s -> p.fiatReturn().getOrElse(FungibleData.zero(USDCurrency)))
    }.sumByKey()

  lazy val mistakeContributions: Map[Mistake, FungibleData] =
    positions.items.collect {
      case p if p.journal.isDefined => p.journal.get -> p
    }.flatMap {
      case (journal, p) =>
        journal.mistakes.map(s => s -> p.fiatReturn().getOrElse(FungibleData.zero(USDCurrency)))
    }.sumByKey()

  private val USDCurrency: Currency = refineV.unsafeFrom("USD")
}

object PortfolioKpi {
  implicit class FungibleDataOps[Key](items: List[(Key, FungibleData)]) {
    def sumByKey(): Map[Key, FungibleData] =
      items.groupBy(_._1).view.mapValues(_.map(_._2).sumFungibleData()).toMap
  }
}
