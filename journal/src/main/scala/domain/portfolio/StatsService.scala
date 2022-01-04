package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.error.{
  AccountBalanceComputeError,
  InvalidPortfolioError,
  PortfolioKpiGenerationError,
  StatsError
}
import domain.position.{ MarketPlayService, MarketPlays }
import domain.position.error.MarketPlayError
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.{ KpiFilter, PlayFilter }
import util.{ BeginningOfCrypto, InstantOps }

import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer }

trait StatsService {
  def playsOverview(userWallet: Wallet)(kpiFilter: KpiFilter): IO[StatsError, PlaysOverview]
}

final case class LiveStatsService(
  marketPlaysService: MarketPlayService,
  accountBalance: AccountBalance,
  clock: Clock.Service,
  logger: Logger[String]
) extends StatsService {
  override def playsOverview(wallet: Wallet)(kpiFilter: KpiFilter): IO[StatsError, PlaysOverview] =
    for {
      _                         <- logger.info(s"Fetching KPIs for wallet ${wallet.address}")
      now                       <- clock.instant
      timeInterval              = kpiFilter.interval.getOrElse(TimeInterval(BeginningOfCrypto, now))
      timeIntervalForComparison = intervalForComparePositions(timeInterval)
      count                     = kpiFilter.count.getOrElse(30) //TODO Inspect this!
      filter                    <- PlayFilter(count, timeInterval).toZIO.mapError(InvalidPortfolioError)
      plays                     <- marketPlaysService.getPlays(wallet, filter).mapError(statsErrorMapper)
      referencePlays            <- fetchReferencePlays(wallet, filter, timeIntervalForComparison)
      distinctValues            = new PlaysDistinctValues(plays, filter.interval, referencePlays)
      balanceTrend              <- getBalanceTrend(plays, timeInterval)
      referenceTrend            <- getBalanceTrend(referencePlays, timeIntervalForComparison)
    } yield PlaysOverview(distinctValues, balanceTrend, balanceTrend.performance(referenceTrend))

  /**
   * Creates a new timestamp to be used for retrieving positions that will be used for performance generation.
   * For example, if somebody has a filter with an interval of 5 days, so (t1, now), we need to fetch positions
   * for the following interval: (t2, (t1 - 1)). where t1 needs to be set at the beginning of day, and t1-1 and the end of the day.
   */
  private def intervalForComparePositions(original: TimeInterval): TimeInterval = {
    val newInterval = original.minus(refineV.unsafeFrom(original.dayCount.value + 1))
    val newStart = if (newInterval.start.isAfter(BeginningOfCrypto)) {
      newInterval.start.atBeginningOfDay()
    } else BeginningOfCrypto
    newInterval.copy(start = newStart, end = newInterval.end.atEndOfDay())
  }

  private def fetchReferencePlays(
    wallet: Wallet,
    filter: PlayFilter,
    referenceInterval: TimeInterval
  ): IO[StatsError, MarketPlays] =
    if (filter.interval.start == BeginningOfCrypto) {
      marketPlaysService
        .getPlays(
          wallet,
          filter.copy(interval = referenceInterval)
        )
        .mapError(statsErrorMapper)
    } else UIO(MarketPlays.empty())

  private def getBalanceTrend(plays: MarketPlays, interval: TimeInterval) =
    accountBalance
      .trend(plays, interval)
      .mapBoth(
        _ => AccountBalanceComputeError(s"Error computing account balance for interval $interval"),
        dataPoints => Trend(dataPoints).right.get
      )

  private def statsErrorMapper(positionError: MarketPlayError): StatsError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveStatsService {
  lazy val layer: URLayer[Has[MarketPlayService] with Has[AccountBalance] with Clock with Logging, Has[StatsService]] =
    (LiveStatsService(_, _, _, _)).toLayer
}
