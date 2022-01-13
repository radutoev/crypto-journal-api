package io.softwarechain.cryptojournal
package infrastructure.journal.service

import domain.model.{ UserId, WalletAddress }
import domain.position.MarketPlayService
import domain.wallet.error.{ UnableToAddWallet, WalletError }
import domain.wallet.model.{ ImportDone, WalletImportStatus }
import domain.wallet.{ UserWalletRepo, Wallet, WalletRepo, WalletService }
import infrastructure.sync.SyncFacade

import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, URLayer }

final case class LiveWalletService(
  userWalletRepo: UserWalletRepo,
  walletRepo: WalletRepo,
  positionService: MarketPlayService,
  syncFacade: SyncFacade,
  logger: Logger[String]
) extends WalletService {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    val userWallet = Wallet(userId, address)

    //TODO Recovery code!
    userWalletRepo.addWallet(userId, address).zipRight {
      walletRepo.exists(address).flatMap {
        case true => logger.info(s"Address ${address.value} found in system. Skipping import.")
        case false =>
          (
            //TODO Add back the sync call after deploying the sync module
            walletRepo
              .addWallet(address) //*> syncFacade.addWallet(address).orElseFail(UnableToAddWallet(address))
            )
            .zipParRight(
              positionService
                .importPlays(userWallet)
                .tapError(_ => logger.error(s"Unable to import positions for $address"))
                .zipRight(walletRepo.updateImportStatus(address, ImportDone))
                .ignore
                .forkDaemon
            )
            .unit
      }
    }
  }

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = userWalletRepo.getWallets(userId)

  override def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
    userWalletRepo.removeWallet(userId, address)

  override def getImportStatus(address: WalletAddress): IO[WalletError, WalletImportStatus] =
    walletRepo.getImportStatus(address)
}

object LiveWalletService {
  lazy val layer: URLayer[Has[UserWalletRepo] with Has[WalletRepo] with Has[
    MarketPlayService
  ] with Has[SyncFacade] with Logging, Has[
    WalletService
  ]] =
    (LiveWalletService(_, _, _, _, _)).toLayer
}
