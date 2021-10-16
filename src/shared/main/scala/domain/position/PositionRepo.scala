package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ ContextId, Currency, State, WalletAddress }
import domain.position.Position.PositionId
import domain.position.error.PositionError
import vo.filter.PositionFilter
import vo.pagination.Page

import zio.{ Has, IO, Task, ZIO }

import java.time.Instant

trait PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(address: WalletAddress, filter: PositionFilter): IO[PositionError, List[Position]]

  def getPositions(
    address: WalletAddress,
    filter: PositionFilter,
    contextId: ContextId
  ): IO[PositionError, Page[Positions]] //TODO maye add a type Page.

  def getPositions(address: WalletAddress, startFrom: Instant): IO[PositionError, List[Position]]

  def getPositions(address: WalletAddress, state: State): IO[PositionError, List[Position]]

  def getPosition(positionId: PositionId): IO[PositionError, Position]

  def getLatestPosition(address: WalletAddress, currency: Currency): IO[PositionError, Option[Position]]
}

object PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(address, positions))

  def getLatestPosition(
    address: WalletAddress,
    currency: Currency
  ): ZIO[Has[PositionRepo], PositionError, Option[Position]] =
    ZIO.serviceWith[PositionRepo](_.getLatestPosition(address, currency))
}
