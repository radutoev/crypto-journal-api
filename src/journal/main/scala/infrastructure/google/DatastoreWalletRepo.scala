package io.softwarechain.cryptojournal
package infrastructure.google

import config.DatastoreConfig
import domain.model.WalletAddress
import domain.wallet.error.{ UnableToAddWallet, WalletFetchError }
import domain.wallet.{ error, WalletRepo }

import com.google.cloud.datastore.{ Datastore, DatastoreException, Entity, ReadOption }
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer }

final case class DatastoreWalletRepo(datastore: Datastore, datastoreConfig: DatastoreConfig, logger: Logger[String])
    extends WalletRepo {
  override def addWallet(address: WalletAddress): IO[error.WalletError, Unit] =
    logger.info(s"Insert address $address") *>
      Task(
        datastore
          .put(
            Entity.newBuilder(datastore.newKeyFactory().setKind(datastoreConfig.wallet).newKey(address.value)).build()
          )
      ).tapError(err => logger.warn(err.toString))
        .orElseFail(UnableToAddWallet(address))
        .unit

  override def exists(address: WalletAddress): IO[error.WalletError, Boolean] =
    Task(
      datastore.get(
        datastore.newKeyFactory().setKind(datastoreConfig.wallet).newKey(address.value),
        Seq.empty[ReadOption]: _*
      )
    ).mapBoth({
        case ex: DatastoreException => WalletFetchError(address, ex)
        case t: Throwable           => WalletFetchError(address, t)
      }, Option(_))
      .map(_.isDefined)
}

object DatastoreWalletRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging, Has[WalletRepo]] =
    (DatastoreWalletRepo(_, _, _)).toLayer
}
