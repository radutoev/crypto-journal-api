package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.{ WalletAddress, WalletAddressPredicate }
import domain.wallet.error.{ InvalidWallet, UnableToAddWallet, WalletError, WalletFetchError, WalletsFetchError }
import domain.wallet.model.{ Importing, WalletImportStatus }
import domain.wallet.{ error, WalletImportRepo }

import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore.{ Datastore, DatastoreException, Entity, Key, Query, QueryResults, ReadOption }
import eu.timepit.refined
import io.softwarechain.cryptojournal.util.tryOrLeft
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, Task, URLayer }

import scala.jdk.CollectionConverters._

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
              .set("importStatus", Importing.toString)
              .build()
          )
      ).tapError(err => logger.warn(err.toString))
        .orElseFail(UnableToAddWallet(address))
        .unit

  override def getByImportStatus(status: WalletImportStatus): IO[WalletError, List[WalletAddress]] =
    logger.info(s"Fetch wallets with import status $status") *>
      executeQuery(
        Query
          .newEntityQueryBuilder()
          .setKind(datastoreConfig.wallet)
          .setFilter(PropertyFilter.eq("importStatus", status.toString))
          .build()
      ).mapBoth(
        throwable => WalletsFetchError(throwable),
        queryResult =>
          queryResult.asScala.toList.map(asWalletAddress).collect {
            case Right(value) => value
          }
      )

  private def asWalletAddress(entity: Entity): Either[InvalidWallet, WalletAddress] =
    tryOrLeft(entity.getKey.getName, InvalidWallet("Entity has no key name"))
      .flatMap(rawIdStr =>
        refined
          .refineV[WalletAddressPredicate](rawIdStr)
          .left
          .map(_ => InvalidWallet(s"Invalid format for id $rawIdStr"))
      )

  private def executeQuery[Result](query: Query[Result]): Task[QueryResults[Result]] =
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .tapError(throwable => logger.warn(throwable.getMessage))

  override def exists(address: WalletAddress): IO[error.WalletError, Boolean] =
    getByAddress(address).map(Option(_).isDefined)

  override def updateImportStatus(address: WalletAddress, state: WalletImportStatus): IO[error.WalletError, Unit] =
    logger.info(s"Set import status for ${address.value} to $state") *>
      Task(datastore.update(Entity.newBuilder(addressKey(address)).set("importStatus", state.toString).build()))
        .mapError(err => datastoreErrorMapper(address, err))
        .unit

  override def getImportStatus(address: WalletAddress): IO[error.WalletError, WalletImportStatus] =
    logger.info(s"Fetch import state for ${address.value}") *>
      getByAddress(address).map(entity => WalletImportStatus.unsafeApply(entity.getString("importStatus")))

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