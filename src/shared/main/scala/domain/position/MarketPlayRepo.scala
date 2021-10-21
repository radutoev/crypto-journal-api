package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ContextId, Currency, PlayId, State, WalletAddress}
import domain.position.error.MarketPlayError
import vo.filter.PlayFilter
import vo.pagination.Page

import zio.{Has, IO, Task, ZIO}

import java.time.Instant

trait MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): Task[Unit]

  def getPlays(address: WalletAddress, filter: PlayFilter): IO[MarketPlayError, List[Position]]

  def getPositions(
                    address: WalletAddress,
                    filter: PlayFilter,
                    contextId: ContextId
  ): IO[MarketPlayError, Page[Positions]]

  def getPlays(address: WalletAddress, startFrom: Instant): IO[MarketPlayError, List[Position]]

  def getPlays(address: WalletAddress, state: State): IO[MarketPlayError, List[Position]]

  def getPlay(playId: PlayId): IO[MarketPlayError, Position]

  def getLatestPlay(address: WalletAddress, currency: Currency): IO[MarketPlayError, Option[Position]]
}

object MarketPlayRepo {
  def save(address: WalletAddress, positions: List[MarketPlay]): ZIO[Has[MarketPlayRepo], Throwable, Unit] =
    ZIO.serviceWith[MarketPlayRepo](_.save(address, positions))

  def getLatestPlay(
    address: WalletAddress,
    currency: Currency
  ): ZIO[Has[MarketPlayRepo], MarketPlayError, Option[Position]] =
    ZIO.serviceWith[MarketPlayRepo](_.getLatestPlay(address, currency))
}
