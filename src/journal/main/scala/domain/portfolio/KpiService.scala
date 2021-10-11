package io.softwarechain.cryptojournal
package domain.portfolio

import domain.position.PositionService
import domain.wallet.Wallet
import vo.filter.{ KpiFilter, PositionFilter }

import zio.clock.Clock
import zio.{ Has, Task, URLayer }

trait KpiService {
  def portfolioKpi(userWallet: Wallet)(kpiFilter: KpiFilter): Task[PortfolioKpi]
}

final case class LiveKpiService(positionService: PositionService, clock: Clock.Service) extends KpiService {
  //TODO Add error io.softwarechain.cryptojournal.domain, and handle empty positions case with NoData error or something.
  override def portfolioKpi(userWallet: Wallet)(kpiFilter: KpiFilter): Task[PortfolioKpi] =
    PositionFilter(Int.MaxValue, kpiFilter.interval).toZIO
      .orElseFail(new RuntimeException("Invalid position filter"))
      .flatMap { filter =>
        positionService
          .getPositions(userWallet, filter)
          .mapBoth(_ => new RuntimeException("failed"), new PortfolioKpi(_, filter.interval))
      }
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock, Has[KpiService]] = (LiveKpiService(_, _)).toLayer
}
