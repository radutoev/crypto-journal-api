package io.softwarechain.cryptojournal
package application

import domain.account.UserContext
import domain.model._
import domain.position.{Position, PositionService}
import domain.portfolio.{KpiService, PortfolioKpi}
import domain.wallet.{Wallet, WalletService}
import domain.wallet.error.WalletError

import zio.{Has, ZIO}

object CryptoJournalApi {
  def getPositions(address: WalletAddress): ZIO[Has[PositionService] with Has[UserContext], Throwable, List[Position]] =
    for {
      userId    <- UserContext.userId
      positions <- ZIO.serviceWith[PositionService](_.getPositions(UserWallet(userId, address)))
    } yield positions

  def getPortfolioKpis(address: WalletAddress): ZIO[Has[KpiService] with Has[UserContext], Throwable, PortfolioKpi] =
    for {
      userId <- UserContext.userId
      portfolioKpi <- ZIO.serviceWith[KpiService](_.portfolioKpi(UserWallet(userId, address)))
    } yield portfolioKpi

  def addWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[UserContext], WalletError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[WalletService](_.addWallet(userId, address))
    } yield ()

  def removeWallet(address: WalletAddress) = {
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[WalletService](_.removeWallet(userId, address))
    } yield ()
  }

  def getWallets(): ZIO[Has[WalletService] with Has[UserContext], WalletError, List[Wallet]] =
    for {
      userId  <- UserContext.userId
      wallets <- ZIO.serviceWith[WalletService](_.getWallets(userId))
    } yield wallets
}
