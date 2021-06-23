package io.softwarechain.cryptojournal
package domain.position

import domain.model.{UserId, WalletAddress}

import zio.{Has, Task, ZIO}

trait PositionRepo {
  def save(address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(address: WalletAddress): Task[List[Position]]

  /**
   * Checks if the system is aware of the given address.
   *
   * @param address to lookup
   * @return true if system is aware of the wallet address, false otherwise.
   */
  def exists(address: WalletAddress): Task[Boolean]
}

object PositionRepo {
  def save(userId: UserId, address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(address, positions))

  def getPositions(userId: UserId, wallet: WalletAddress): ZIO[Has[PositionRepo], Throwable, List[Position]] =
    ZIO.serviceWith[PositionRepo](_.getPositions(wallet))
}
