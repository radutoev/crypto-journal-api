package io.softwarechain.cryptojournal
package infrastructure.google

import config.DatastoreConfig
import domain.model.WalletAddress
import domain.wallet.error.{UnableToAddWallet, WalletError, WalletFetchError}
import domain.wallet.model.{Importing, WalletImportState}
import domain.wallet.{WalletImportRepo, error}

import com.google.cloud.datastore._
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer}

final case class DatastoreWalletImportRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  logger: Logger[String]
) extends WalletImportRepo {
  override def addWallet(address: WalletAddress): IO[error.WalletError, Unit] =
    logger.info(s"Insert address $address") *>
      Task(
        datastore
          .put(
            Entity
              .newBuilder(addressKey(address))
              .set("importState", Importing.toString)
              .build()
          )
      ).tapError(err => logger.warn(err.toString))
        .orElseFail(UnableToAddWallet(address))
        .unit

  override def exists(address: WalletAddress): IO[error.WalletError, Boolean] =
    getByAddress(address).map(Option(_).isDefined)

  override def updateImportStatus(address: WalletAddress, state: WalletImportState): IO[error.WalletError, Unit] =
    Task(datastore.update(Entity.newBuilder(addressKey(address)).set("importState", state.toString).build()))
      .mapError(err => datastoreErrorMapper(address, err))
      .unit

  override def getImportState(address: WalletAddress): IO[error.WalletError, WalletImportState] =
    logger.info(s"Fetch import state for ${address.value}") *>
      getByAddress(address).map(entity => WalletImportState.unsafeApply(entity.getString("importState")))

  private def getByAddress(address: WalletAddress): IO[WalletError, Entity] =
    Task(
      datastore.get(
        addressKey(address),
        Seq.empty[ReadOption]: _*
      )
    ).mapError(err => datastoreErrorMapper(address, err))

  private def addressKey: WalletAddress => Key =
    address => datastore.newKeyFactory().setKind(datastoreConfig.wallet).newKey(address.value)

  private def datastoreErrorMapper(address: WalletAddress, err: Throwable): WalletError = err match {
    case ex: DatastoreException => WalletFetchError(address, ex)
    case t: Throwable           => WalletFetchError(address, t)
  }
}

object DatastoreWalletImportRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging, Has[WalletImportRepo]] =
    (DatastoreWalletImportRepo(_, _, _)).toLayer
}
