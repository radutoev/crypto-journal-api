package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.UserWallet
import domain.position.PositionService

import zio.clock.Clock
import zio.{Has, Task, URLayer}

trait KpiService {
  //TODO Add time-interval.
  def portfolioKpi(userWallet: UserWallet): Task[PortfolioKpi]
}

final case class LiveKpiService(positionService: PositionService,
                                clock: Clock.Service) extends KpiService {
  override def portfolioKpi(userWallet: UserWallet): Task[PortfolioKpi] = {
    for {
      instant <- clock.instant
      positions <- positionService.getPositions(userWallet)
    } yield new PortfolioKpi(positions)
  }
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock, Has[KpiService]] = (LiveKpiService(_, _)).toLayer
}
