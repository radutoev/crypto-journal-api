package io.softwarechain.cryptojournal
package domain.portfolio

import domain.portfolio.error.{ PortfolioError, PortfolioKpiGenerationError }
import domain.position.PositionService
import domain.position.error.PositionError
import domain.wallet.Wallet
import vo.filter.{ KpiFilter, PositionFilter }

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
      positions          <- positionService.getPositions(wallet, filter).mapError(portfolioErrorMapper)
      referencePositions <- positionService.getPositions(wallet, filter).mapError(portfolioErrorMapper)
    } yield new PortfolioKpi(positions, filter.interval, referencePositions)

  private def portfolioErrorMapper(positionError: PositionError): PortfolioError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock with Logging, Has[KpiService]] =
    (LiveKpiService(_, _, _)).toLayer
}
