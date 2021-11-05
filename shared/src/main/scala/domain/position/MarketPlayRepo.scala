package io.softwarechain.cryptojournal
package domain.position

import domain.position.error.MarketPlayError
import vo.filter.PlayFilter
import vo.pagination.Page

import io.softwarechain.cryptojournal.domain.model.{ContextId, Currency, PlayId, WalletAddress}
import zio.{Has, IO, Task, ZIO}

trait MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): Task[Unit]

  def getPlays(address: WalletAddress, filter: PlayFilter): IO[MarketPlayError, List[MarketPlay]]

  def getPlays(
    address: WalletAddress,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, Page[MarketPlays]]

  def getPositions(address: WalletAddress, state: State): IO[MarketPlayError, List[Position]]

  def getPosition(playId: PlayId): IO[MarketPlayError, Position]

  def getLatestPosition(address: WalletAddress, currency: Currency): IO[MarketPlayError, Option[Position]]
}

object MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): ZIO[Has[MarketPlayRepo], Throwable, Unit] =
    ZIO.serviceWith[MarketPlayRepo](_.save(address, positions))

  def getLatestPosition(
    address: WalletAddress,
    currency: Currency
  ): ZIO[Has[MarketPlayRepo], MarketPlayError, Option[Position]] =
    ZIO.serviceWith[MarketPlayRepo](_.getLatestPosition(address, currency))
}
