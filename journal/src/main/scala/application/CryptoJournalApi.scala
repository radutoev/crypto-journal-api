package io.softwarechain.cryptojournal
package application

import domain.account.RequestContext
import domain.market.MarketService
import domain.market.error.MarketError
import domain.model.{Ohlcv, PlayId, WalletAddress}
import domain.portfolio.error.StatsError
import domain.portfolio.model.{BinData, BinName, NetReturnDistributionByDay, PlaysGrouping, TradeSummary}
import domain.portfolio.{PlaysOverview, StatsService}
import domain.position._
import domain.position.error.MarketPlayError
import domain.wallet.error.WalletError
import domain.wallet.model.WalletImportStatus
import domain.wallet.{Wallet, WalletService}
import vo.TimeInterval
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
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, PositionDetails[Position]] =
    for {
      userId   <- RequestContext.userId
      position <- ZIO.serviceWith[MarketPlayService](_.getPositionDetails(userId, positionId))
    } yield position

  def getNextPositions(
    positionId: PlayId
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, List[Position]] =
    ZIO.serviceWith[MarketPlayService](_.getNextPositions(positionId))

  def getPreviousPositions(
    positionId: PlayId
  ): ZIO[Has[MarketPlayService] with Has[RequestContext], MarketPlayError, List[Position]] =
    ZIO.serviceWith[MarketPlayService](_.getPreviousPositions(positionId))

  def getPlaysOverview(
    address: WalletAddress,
    kpiFilter: KpiFilter
  ): ZIO[Has[StatsService] with Has[RequestContext], StatsError, PlaysOverview] =
    for {
      userId       <- RequestContext.userId
      portfolioKpi <- ZIO.serviceWith[StatsService](_.playsOverview(Wallet(userId, address), kpiFilter))
    } yield portfolioKpi

  def getTradeSummary(address: WalletAddress, kpiFilter: KpiFilter): ZIO[Has[StatsService] with Has[RequestContext], StatsError, TradeSummary] = {
    for {
      userId <- RequestContext.userId
      data   <- ZIO.serviceWith[StatsService](_.tradesSummary(Wallet(userId, address), kpiFilter))
    } yield data
  }

  def aggregatePlays(address: WalletAddress, interval: TimeInterval, grouping: PlaysGrouping): ZIO[Has[StatsService] with Has[RequestContext], StatsError, Map[BinName, BinData]] =
    for {
      userId  <- RequestContext.userId
      data    <- ZIO.serviceWith[StatsService](_.playsDistribution(Wallet(userId, address), interval, grouping))
        .map(_.view.mapValues(_.binData).toMap)
    } yield data

  def getMonthlyNetReturnDistribution(
    address: WalletAddress,
    interval: TimeInterval
  ): ZIO[Has[StatsService] with Has[RequestContext], StatsError, NetReturnDistributionByDay] =
    for {
      userId       <- RequestContext.userId
      distribution <- ZIO.serviceWith[StatsService](_.netReturnDistributionByDay(Wallet(userId, address), interval))
    } yield distribution

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

  def getHistoricalOhlcv(playId: PlayId): ZIO[Has[MarketService], MarketError, List[Ohlcv]] =
    ZIO.serviceWith[MarketService](_.getHistoricalOhlcv(playId))
}
