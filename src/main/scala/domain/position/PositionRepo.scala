package io.softwarechain.cryptojournal
package domain.position

import domain.model.WalletAddress
import domain.position.Position.PositionId
import domain.position.error._
import vo.TimeInterval

import eu.timepit.refined.types.numeric.PosInt
import zio.{Has, IO, Task, ZIO}

trait PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(address: WalletAddress)(implicit count: PosInt): IO[PositionError, List[Position]]

  def getPositions(address: WalletAddress, timeInterval: TimeInterval): Task[List[Position]]

  def getPosition(positionId: PositionId): IO[PositionError, Position]

  /**
   * Checks if the system is aware of the given address.
   *
   * @param address to lookup
   * @return true if system is aware of the wallet address, false otherwise.
   */
  def exists(address: WalletAddress): Task[Boolean]

  def getCheckpoint(address: WalletAddress): IO[PositionError, Checkpoint]
}

object PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(address, positions))

  def getPositions(wallet: WalletAddress)(implicit count: PosInt): ZIO[Has[PositionRepo], PositionError, List[Position]] =
    ZIO.serviceWith[PositionRepo](_.getPositions(wallet))
}
