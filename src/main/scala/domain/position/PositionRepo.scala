package io.softwarechain.cryptojournal
package domain.position

import domain.model.{UserId, WalletAddress}

import zio.{Has, Task, ZIO}

trait PositionRepo {
  def save(userId: UserId, address: WalletAddress, positions: List[Position]): Task[Unit]

  def getPositions(userId: UserId, address: WalletAddress): Task[List[Position]]
}

object PositionRepo {
  def save(userId: UserId, address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(userId, address, positions))

  def getPositions(userId: UserId, wallet: WalletAddress): ZIO[Has[PositionRepo], Throwable, List[Position]] =
    ZIO.serviceWith[PositionRepo](_.getPositions(userId, wallet))
}
