package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{ UserId, UserWallet, WalletAddress }
import domain.position.PositionService
import domain.position.error._
import domain.wallet.error.WalletError

import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer }

trait WalletService {
  def addWallet(userId: UserId, walletAddress: WalletAddress): IO[WalletError, Unit]

  def getWallets(userId: UserId): IO[WalletError, List[Wallet]]

  def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit]
}

final case class LiveWalletService(walletRepo: WalletRepo, positionService: PositionService, logger: Logger[String])
    extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
//    val userWallet = UserWallet(userId, address)
    walletRepo.addWallet(userId, address)

//    walletRepo
//      .addWallet(userId, address)
//      .zipParRight {
//        positionService
//          .getCheckpoint(address)
//          .foldM(
//            {
//              case CheckpointNotFound(address) =>
//                logger.info(s"No checkpoint found for ${address.value} Performing full import...") *>
//                  positionService.importPositions(userWallet)
//              case _ => logger.error(s"Unable to fetch latest checkpoint. Aborting address ${address.value} import")
//            },
//            checkpoint =>
//              checkpoint.latestTxTimestamp.fold[Task[Unit]](logger.debug("Import job currently running. Skipping..."))(
//                timestamp =>
//                  logger.info(s"Importing positions starting from ${checkpoint.latestTxTimestamp.get}") *>
//                    positionService
//                      .importPositions(userWallet, timestamp)
//                      .tapError(_ => logger.error(s"Unable to import positions for $address"))
//                      .ignore
//              )
//          )
//          .forkDaemon
//      }
//      .unit

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = walletRepo.getWallets(userId)

  override def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
    walletRepo.removeWallet(userId, address)
}

object LiveWalletService {
  lazy val layer: URLayer[Has[WalletRepo] with Has[PositionService] with Logging, Has[WalletService]] =
    (LiveWalletService(_, _, _)).toLayer
}
