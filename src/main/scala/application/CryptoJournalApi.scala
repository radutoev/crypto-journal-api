package io.softwarechain.cryptojournal
package application

import domain.account.UserContext
import domain.model._
import domain.portfolio.{KpiService, PortfolioKpi}
import domain.position.{ JournalPosition, JournalPositions }
import domain.position.Position.PositionId
import domain.position.error.PositionError
import domain.position.{JournalEntry, JournalingService, PositionService, Positions, TagPositions}
import domain.wallet.error.WalletError
import domain.wallet.{Wallet, WalletService}
import vo.filter.{KpiFilter, PositionFilter}

import zio.{Has, ZIO}

object CryptoJournalApi {
  def getPositions(
    address: WalletAddress,
    filter: PositionFilter
  ): ZIO[Has[PositionService] with Has[UserContext], PositionError, JournalPositions] =
    for {
      userId    <- UserContext.userId
      positions <- ZIO.serviceWith[PositionService](_.getJournalPositions(UserWallet(userId, address))(filter))
    } yield positions

  def getPosition(
    positionId: PositionId
  ): ZIO[Has[PositionService] with Has[UserContext], PositionError, JournalPosition] =
    for {
      userId   <- UserContext.userId
      position <- ZIO.serviceWith[PositionService](_.getPosition(userId, positionId))
    } yield position

  def diff(address: WalletAddress): ZIO[Has[PositionService] with Has[UserContext], PositionError, Positions] =
    for {
      userId   <- UserContext.userId
      position <- ZIO.serviceWith[PositionService](_.diff(UserWallet(userId, address)))
    } yield position

  def getPortfolioKpis(
    address: WalletAddress
  )(kpiFilter: KpiFilter): ZIO[Has[KpiService] with Has[UserContext], Throwable, PortfolioKpi] =
    for {
      userId       <- UserContext.userId
      portfolioKpi <- ZIO.serviceWith[KpiService](_.portfolioKpi(UserWallet(userId, address))(kpiFilter))
    } yield portfolioKpi

  def addWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[UserContext], WalletError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[WalletService](_.addWallet(userId, address))
    } yield ()

  def removeWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[UserContext], WalletError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[WalletService](_.removeWallet(userId, address))
    } yield ()

  def getWallets(): ZIO[Has[WalletService] with Has[UserContext], WalletError, List[Wallet]] =
    for {
      userId  <- UserContext.userId
      wallets <- ZIO.serviceWith[WalletService](_.getWallets(userId))
    } yield wallets

  def saveJournalEntry(
    positionId: PositionId,
    entry: JournalEntry
  ): ZIO[Has[JournalingService] with Has[UserContext], PositionError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntry(userId, positionId, entry))
    } yield ()

  def addSetups(tagPositions: TagPositions): ZIO[Has[JournalingService] with Has[UserContext], PositionError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.addSetups(userId, tagPositions))
    } yield ()

  def addMistakes(tagPositions: TagPositions): ZIO[Has[JournalingService] with Has[UserContext], PositionError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.addMistakes(userId, tagPositions))
    } yield ()
}
