package io.softwarechain.cryptojournal
package application

import domain.account.UserContext
import domain.market.error.MarketError
import domain.market.{ MarketService, Ohlcv }
import domain.model.{ UserWallet, WalletAddress }
import domain.portfolio.{ KpiService, PortfolioKpi }
import domain.position.Position.PositionId
import domain.position.error.PositionError
import domain.position._
import domain.wallet.error.WalletError
import domain.wallet.{ Wallet, WalletService }
import vo.filter.{ KpiFilter, PositionFilter }

import zio.{ Has, ZIO }

object CryptoJournalApi {
  def getPositions(
    address: WalletAddress,
    filter: PositionFilter
  ): ZIO[Has[PositionService] with Has[UserContext], PositionError, Positions] =
    for {
      userId    <- UserContext.userId
      positions <- ZIO.serviceWith[PositionService](_.getPositions(UserWallet(userId, address))(filter))
    } yield positions

  def getPosition(
    positionId: PositionId
  ): ZIO[Has[PositionService] with Has[UserContext], PositionError, Position] =
    for {
      userId   <- UserContext.userId
      position <- ZIO.serviceWith[PositionService](_.getPosition(userId, positionId))
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

  def saveJournalEntries(
    positionEntries: List[PositionJournalEntry]
  ): ZIO[Has[JournalingService] with Has[UserContext], PositionError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntries(userId, positionEntries))
    } yield ()

  def getHistoricalOhlcv(): ZIO[Has[MarketService], MarketError, List[Ohlcv]] =
    for {
      data <- ZIO.serviceWith[MarketService](_.getHistoricalOhlcv())
    } yield data
}
