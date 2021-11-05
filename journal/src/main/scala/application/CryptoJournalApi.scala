package io.softwarechain.cryptojournal
package application

import domain.account.RequestContext
import domain.market.error.MarketError
import domain.market.{MarketService, Ohlcv}
import domain.model.{PlayId, WalletAddress}
import domain.portfolio.error.PortfolioError
import domain.portfolio.{KpiService, PortfolioKpi}
import domain.position._
import domain.position.error.MarketPlayError
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus
import domain.wallet.{Wallet, WalletService}
import vo.filter.{KpiFilter, PlayFilter}

import zio.{Has, ZIO}

object CryptoJournalApi {
  def getLatestPlays(
    address: WalletAddress,
    filter: PlayFilter
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, MarketPlays] =
    for {
      userId    <- RequestContext.userId
      positions <- ZIO.serviceWith[MarketPlayService](_.getPlays(Wallet(userId, address), filter))
    } yield positions

  def getPlays(
    address: WalletAddress,
    filter: PlayFilter
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, MarketPlays] =
    for {
      userId    <- RequestContext.userId
      contextId <- RequestContext.contextId
      positions <- ZIO.serviceWith[MarketPlayService](_.getPlays(Wallet(userId, address), filter, contextId))
    } yield positions

  def getPosition(
    positionId: PlayId
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, Position] =
    for {
      userId   <- RequestContext.userId
      position <- ZIO.serviceWith[MarketPlayService](_.getPosition(userId, positionId))
    } yield position

  def getPortfolioKpis(
    address: WalletAddress
  )(kpiFilter: KpiFilter): ZIO[Has[KpiService] with Has[RequestContext], PortfolioError, PortfolioKpi] =
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
    positionId: PlayId,
    entry: JournalEntry
  ): ZIO[Has[JournalingService] with Has[RequestContext], MarketPlayError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntry(userId, positionId, entry))
    } yield ()

  def saveJournalEntries(
    positionEntries: List[PositionJournalEntry]
  ): ZIO[Has[JournalingService] with Has[RequestContext], MarketPlayError, Unit] =
    for {
      userId <- RequestContext.userId
      _      <- ZIO.serviceWith[JournalingService](_.saveJournalEntries(userId, positionEntries))
    } yield ()

  def getHistoricalOhlcv: ZIO[Has[MarketService], MarketError, List[Ohlcv]] =
    for {
      data <- ZIO.serviceWith[MarketService](_.getHistoricalOhlcv())
    } yield data
}
