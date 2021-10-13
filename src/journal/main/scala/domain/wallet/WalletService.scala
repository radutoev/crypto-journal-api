package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{UserId, WalletAddress}
import domain.position.PositionService
import domain.wallet.error.WalletError
import domain.wallet.model.{ImportDone, WalletImportStatus}

import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]

  def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus]
}

final case class LiveWalletService(
  userWalletRepo: UserWalletRepo,
  walletRepo: WalletImportRepo,
  positionService: PositionService,
  logger: Logger[String]
) extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    val userWallet = Wallet(userId, address)
    walletRepo.exists(address).flatMap {
      case true => logger.info(s"Address ${address.value} found in system. Skipping import.")
      case false =>
        (userWalletRepo
          .addWallet(userId, address) *> walletRepo.addWallet(address)) //TODO Recover code, not sure if saga
          .zipParRight(
            positionService
              .importPositions(userWallet)
              .tapError(_ => logger.error(s"Unable to import positions for $address"))
              .zipRight(walletRepo.updateImportStatus(address, ImportDone))
              .ignore
              .forkDaemon
          )
          .unit
    }
  }

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = userWalletRepo.getWallets(userId)

  override def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
    userWalletRepo.removeWallet(userId, address)

  override def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus] =
    walletRepo.getImportStatus(address)
}

object LiveWalletService {
  lazy val layer: URLayer[Has[UserWalletRepo] with Has[WalletImportRepo] with Has[
    PositionService
  ] with Logging, Has[
    WalletService
  ]] =
    (LiveWalletService(_, _, _, _)).toLayer
}
