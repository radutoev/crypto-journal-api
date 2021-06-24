package io.softwarechain.cryptojournal
package domain.position

import domain.model.WalletAddress

import vo.TimeInterval

import eu.timepit.refined.types.numeric.PosInt
import zio.{Has, Task, ZIO}

trait PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(address: WalletAddress)(implicit count: PosInt): Task[List[Position]]

  def getPositions(address: WalletAddress, timeInterval: TimeInterval): Task[List[Position]]

  /**
   * Checks if the system is aware of the given address.
   *
   * @param address to lookup
   * @return true if system is aware of the wallet address, false otherwise.
   */
  def exists(address: WalletAddress): Task[Boolean]
}

object PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(address, positions))

  def getPositions(wallet: WalletAddress)(implicit count: PosInt): ZIO[Has[PositionRepo], Throwable, List[Position]] =
    ZIO.serviceWith[PositionRepo](_.getPositions(wallet))
}
