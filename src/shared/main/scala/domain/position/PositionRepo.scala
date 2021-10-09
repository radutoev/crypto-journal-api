package io.softwarechain.cryptojournal
package domain.position

import domain.model.{State, WalletAddress}
import domain.position.Position.PositionId
import domain.position.error.PositionError
import vo.filter.PositionFilter

import zio.{Has, IO, Task, ZIO}

import java.time.Instant

trait PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(address: WalletAddress)(filter: PositionFilter): IO[PositionError, List[Position]]

  def getPositions(address: WalletAddress, startFrom: Instant): IO[PositionError, List[Position]]

  def getPositions(address: WalletAddress, state: State): IO[PositionError, List[Position]]

  def getPosition(positionId: PositionId): IO[PositionError, Position]
}

object PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(address, positions))
}
