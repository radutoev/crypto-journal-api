package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.error.{ PortfolioError, PortfolioKpiGenerationError }
import domain.position.{ PositionService, Positions }
import domain.position.error.PositionError
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.{ KpiFilter, PositionFilter }
import util.{ BeginningOfCrypto, InstantOps }

import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer }

import java.time.Instant

trait KpiService {
  def portfolioKpi(userWallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi]

//  def tagDistribution(wallet: Wallet, kpiFilter: KpiFilter): IO[PortfolioError, TagDistribution]
}

final case class LiveKpiService(positionService: PositionService, clock: Clock.Service, logger: Logger[String])
    extends KpiService {
  override def portfolioKpi(wallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi] =
    for {
      _                         <- logger.info(s"Generating KPIs for wallet ${wallet.address}")
      now                       <- clock.instant
      timeInterval              = kpiFilter.interval.getOrElse(TimeInterval(BeginningOfCrypto, now))
      timeIntervalForComparison = intervalForComparePositions(timeInterval)
      filter                    <- UIO(PositionFilter(kpiFilter.count, timeInterval))
      positions                 <- positionService.getPositions(wallet, filter).mapError(portfolioErrorMapper)
      referencePositions <- if (timeInterval.start == BeginningOfCrypto) {
                             positionService
                               .getPositions(
                                 wallet,
                                 filter.copy(interval = timeIntervalForComparison)
                               )
                               .mapError(portfolioErrorMapper)
                           } else UIO(Positions.empty())
    } yield new PortfolioKpi(positions, filter.interval, referencePositions)

//  override def tagDistribution(wallet: Wallet, kpiFilter: KpiFilter): IO[PortfolioError, TagDistribution] = {
//    for {
//      portfolioKpi <- getPortfolioKpi(wallet, kpiFilter)
//      portfolioKpiForComparison <-
//    } yield ()
//  }

  private def getPortfolioKpi(wallet: Wallet, kpiFilter: KpiFilter) =
    for {
      now          <- clock.instant
      timeInterval = kpiFilter.interval.getOrElse(TimeInterval(BeginningOfCrypto, now))
      filter       <- UIO(PositionFilter(kpiFilter.count, timeInterval))
      positions    <- positionService.getPositions(wallet, filter).mapError(portfolioErrorMapper)
    } yield PortfolioKpi(positions, filter.interval, Positions.empty())

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

  private def portfolioErrorMapper(positionError: PositionError): PortfolioError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock with Logging, Has[KpiService]] =
    (LiveKpiService(_, _, _)).toLayer
}
