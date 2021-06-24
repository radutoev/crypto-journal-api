package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.UserWallet
import domain.position.PositionService

import vo.TimeInterval

import zio.clock.Clock
import zio.{Has, Task, URLayer}

trait KpiService {
  def portfolioKpi(userWallet: UserWallet, interval: TimeInterval): Task[PortfolioKpi]
}

final case class LiveKpiService(positionService: PositionService,
                                clock: Clock.Service) extends KpiService {
  override def portfolioKpi(userWallet: UserWallet, interval: TimeInterval): Task[PortfolioKpi] = {
    for {
      positions <- positionService.getPositions(userWallet, interval)
    } yield new PortfolioKpi(positions)
  }
}

object LiveKpiService {
  lazy val layer: URLayer[Has[PositionService] with Clock, Has[KpiService]] = (LiveKpiService(_, _)).toLayer
}
