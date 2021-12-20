package io.softwarechain.cryptojournal
package domain.position

import domain.position.error.MarketPlayError
import vo.filter.PlayFilter
import vo.pagination.Page

import io.softwarechain.cryptojournal.domain.model.{ ContextId, Currency, PlayId, WalletAddress }
import zio.{ Has, IO, Task, ZIO }

trait MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): IO[MarketPlayError, Unit]

  def getPlays(address: WalletAddress): IO[MarketPlayError, List[MarketPlay]]

  def getPlays(address: WalletAddress, filter: PlayFilter): IO[MarketPlayError, List[MarketPlay]]

  def getPlays(
    address: WalletAddress,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, Page[MarketPlays]]

  def getPositions(address: WalletAddress, state: State): IO[MarketPlayError, List[Position]]

  def getPositions(playIds: List[PlayId]): IO[MarketPlayError, List[Position]]

  def getPosition(playId: PlayId): IO[MarketPlayError, PositionDetails[PlayId]]

  def getNextPositionIds(playId: PlayId): IO[MarketPlayError, List[PlayId]]

  def getPreviousPositionIds(playId: PlayId): IO[MarketPlayError, List[PlayId]]

  def merge(address: WalletAddress, plays: MarketPlays): IO[MarketPlayError, Unit]
}

object MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): ZIO[Has[MarketPlayRepo], MarketPlayError, Unit] =
    ZIO.serviceWith[MarketPlayRepo](_.save(address, positions))

  def merge(address: WalletAddress, plays: MarketPlays): ZIO[Has[MarketPlayRepo], MarketPlayError, Unit] =
    ZIO.serviceWith[MarketPlayRepo](_.merge(address, plays))
}
