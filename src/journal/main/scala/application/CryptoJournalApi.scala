package io.softwarechain.cryptojournal
package application

import domain.account.RequestContext
import domain.market.error.MarketError
import domain.market.{ MarketService, Ohlcv }
import domain.model.WalletAddress
import domain.portfolio.{ KpiService, PortfolioKpi }
import domain.position.Position.PositionId
import domain.position._
import domain.position.error.PositionError
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus
import domain.wallet.{ Wallet, WalletService }
import vo.filter.{ KpiFilter, PositionFilter }

import zio.{ Has, ZIO }

object CryptoJournalApi {
  def getLatestPositions(
    address: WalletAddress,
    filter: PositionFilter
  ): ZIO[Has[PositionService] with Has[RequestContext], PositionError, Positions] =
    for {
      userId    <- RequestContext.userId
      positions <- ZIO.serviceWith[PositionService](_.getPositions(Wallet(userId, address), filter))
    } yield positions

  def getPositions(
    address: WalletAddress,
    filter: PositionFilter
  ): ZIO[Has[PositionService] with Has[RequestContext], PositionError, Positions] =
    for {
      userId    <- RequestContext.userId
      contextId <- RequestContext.contextId
      positions <- ZIO.serviceWith[PositionService](_.getPositions(Wallet(userId, address), filter, contextId))
    } yield positions

  def getPosition(
    positionId: PositionId
  ): ZIO[Has[PositionService] with Has[RequestContext], PositionError, Position] =
    for {
      userId   <- RequestContext.userId
      position <- ZIO.serviceWith[PositionService](_.getPosition(userId, positionId))
    } yield position

  def getPortfolioKpis(
    address: WalletAddress
  )(kpiFilter: KpiFilter): ZIO[Has[KpiService] with Has[RequestContext], Throwable, PortfolioKpi] =
    for {
      userId       <- RequestContext.userId
      portfolioKpi <- ZIO.serviceWith[KpiService](_.portfolioKpi(Wallet(userId, address))(kpiFilter))
    } yield portfolioKpi

  def addWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[RequestContext], WalletError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[WalletService](_.addWallet(userId, address))
    } yield ()

  def removeWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[RequestContext], WalletError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[WalletService](_.removeWallet(userId, address))
    } yield ()

  def getWallets: ZIO[Has[WalletService] with Has[RequestContext], WalletError, List[Wallet]] =
    for {
      userId  <- RequestContext.userId
      wallets <- ZIO.serviceWith[WalletService](_.getWallets(userId))
    } yield wallets

  def getWalletImportState(address: WalletAddress): ZIO[Has[WalletService], WalletError, WalletImportStatus] =
    ZIO.serviceWith[WalletService](_.getImportStatus(address))

  def saveJournalEntry(
    positionId: PositionId,
    entry: JournalEntry
  ): ZIO[Has[JournalingService] with Has[RequestContext], PositionError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntry(userId, positionId, entry))
    } yield ()

  def saveJournalEntries(
    positionEntries: List[PositionJournalEntry]
  ): ZIO[Has[JournalingService] with Has[RequestContext], PositionError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntries(userId, positionEntries))
    } yield ()

  def getHistoricalOhlcv: ZIO[Has[MarketService], MarketError, List[Ohlcv]] =
    for {
      data <- ZIO.serviceWith[MarketService](_.getHistoricalOhlcv())
    } yield data
}
