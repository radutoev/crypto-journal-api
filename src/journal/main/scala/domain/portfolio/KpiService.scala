package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.error.{ PortfolioError, PortfolioKpiGenerationError }
import domain.position.PositionService
import domain.position.error.PositionError
import domain.wallet.Wallet
import vo.TimeInterval
import vo.filter.{ KpiFilter, PositionFilter }

import util.InstantOps

import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, URLayer }

trait KpiService {
  def portfolioKpi(userWallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi]
}

final case class LiveKpiService(positionService: PositionService, clock: Clock.Service, logger: Logger[String])
    extends KpiService {
  override def portfolioKpi(wallet: Wallet)(kpiFilter: KpiFilter): IO[PortfolioError, PortfolioKpi] =
    for {
      _ <- logger.info(s"Generating KPIs for wallet ${wallet.address}")
      filter <- PositionFilter(Int.MaxValue, kpiFilter.interval).toZIO
                 .orElseFail(PortfolioKpiGenerationError("Invalid position filter"))
      positions <- positionService.getPositions(wallet, filter).mapError(portfolioErrorMapper)
      referencePositions <- positionService
                             .getPositions(wallet, filter.copy(interval = intervalForComparePositions(filter.interval)))
                             .mapError(portfolioErrorMapper)
    } yield new PortfolioKpi(positions, filter.interval, referencePositions)

  /**
   * Creates a new timestamp to be used for retrieving positions that will be used for performance generation.
   * For example, if somebody has a filter with an interval of 5 days, so (t1, now), we need to fetch positions
   * for the following interval: (t2, (t1 - 1)). where t1 needs to be set at the beginning of day, and t1-1 and the end of the day.
   */
  private def intervalForComparePositions(original: TimeInterval): TimeInterval = {
    val newInterval = original.minus(refineV.unsafeFrom(original.dayCount.value + 1))
    newInterval.copy(start = newInterval.start.atBeginningOfDay(), end = newInterval.end.atEndOfDay())
  }

  private def portfolioErrorMapper(positionError: PositionError): PortfolioError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock with Logging, Has[KpiService]] =
    (LiveKpiService(_, _, _)).toLayer
}
