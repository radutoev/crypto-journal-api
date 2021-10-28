package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.error.{InvalidPortfolioError, PortfolioError, PortfolioKpiGenerationError}
import domain.position.{MarketPlayService, MarketPlays}
import domain.position.error.MarketPlayError
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.{KpiFilter, PlayFilter}
import util.{BeginningOfCrypto, InstantOps}

import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, UIO, URLayer}

trait KpiService {
  def portfolioKpi(userWallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi]
}

final case class LiveKpiService(positionService: MarketPlayService, clock: Clock.Service, logger: Logger[String])
    extends KpiService {
  override def portfolioKpi(wallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi] =
    for {
      _                         <- logger.info(s"Generating KPIs for wallet ${wallet.address}")
      now                       <- clock.instant
      timeInterval              = kpiFilter.interval.getOrElse(TimeInterval(BeginningOfCrypto, now))
      timeIntervalForComparison = intervalForComparePositions(timeInterval)
      count                     = kpiFilter.count.getOrElse(30)
      filter                    <- PlayFilter(count, timeInterval).toZIO.mapError(InvalidPortfolioError)
      positions                 <- positionService.getPlays(wallet, filter).mapError(portfolioErrorMapper)
      referencePositions <- if (timeInterval.start == BeginningOfCrypto) {
                             positionService
                               .getPlays(
                                 wallet,
                                 filter.copy(interval = timeIntervalForComparison)
                               )
                               .mapError(portfolioErrorMapper)
                           } else UIO(MarketPlays.empty())
    } yield new PortfolioKpi(positions, filter.interval, referencePositions)

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

  private def portfolioErrorMapper(positionError: MarketPlayError): PortfolioError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveKpiService {
  lazy val layer: URLayer[Has[MarketPlayService] with Clock with Logging, Has[KpiService]] =
    (LiveKpiService(_, _, _)).toLayer
}
