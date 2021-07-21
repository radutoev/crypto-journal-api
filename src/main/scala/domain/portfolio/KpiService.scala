package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.UserWallet
import domain.position.PositionService
import vo.{KpiFilter, PositionFilter}

import zio.clock.Clock
import zio.{Has, Task, URLayer}

trait KpiService {
  def portfolioKpi(userWallet: UserWallet)(kpiFilter: KpiFilter): Task[PortfolioKpi]
}

final case class LiveKpiService(positionService: PositionService, clock: Clock.Service) extends KpiService {
  //TODO Add error domain, and handle empty positions case with NoData error or something.
  override def portfolioKpi(userWallet: UserWallet)(kpiFilter: KpiFilter): Task[PortfolioKpi] = {
    PositionFilter(Int.MaxValue, kpiFilter.interval)
      .toZIO
      .orElseFail(new RuntimeException("Invalid position filter"))
      .flatMap(filter => {
        positionService
          .getPositions(userWallet)(filter)
          .bimap(_ => new RuntimeException("failed"), new PortfolioKpi(_, filter.interval))
      })
  }
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock, Has[KpiService]] = (LiveKpiService(_, _)).toLayer
}
