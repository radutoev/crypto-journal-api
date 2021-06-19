package io.softwarechain.cryptojournal
package application

import domain.account.UserContext
import domain.error.WalletError
import domain.model.WalletAddress
import domain.position.Position
import domain.wallet.WalletService
import service.PositionService

import zio.{Has, ZIO}

object CryptoJournalApi {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[Position]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))

  def addWallet(address: WalletAddress): ZIO[Has[WalletService] with Has[UserContext], WalletError, Unit] =
    for {
      userId <- UserContext.userId
      _      <- ZIO.serviceWith[WalletService](_.addWallet(userId, address))
    } yield ()
}
