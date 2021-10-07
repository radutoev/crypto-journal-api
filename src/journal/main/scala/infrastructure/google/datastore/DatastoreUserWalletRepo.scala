package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.{UserId, WalletAddress, WalletAddressPredicate}
import domain.wallet.error._
import domain.wallet.{UserWalletRepo, Wallet}
import util.{EitherOps, tryOrLeft}

import com.google.cloud.Timestamp
import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

import java.time.Instant
import scala.jdk.CollectionConverters._

final case class DatastoreUserWalletRepo(
  datastore: Datastore,
  datastoreConfig: DatastoreConfig,
  logger: Logger[String],
  clock: Clock.Service
) extends UserWalletRepo {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
    clock.instant.flatMap(instant =>
      getWallet(userId, address)
        .zipRight(ZIO.fail(WalletAddressExists(address)))
        .catchSome {
          case _: WalletNotFound => insertWallet(userId, address)(instant)
        }
    )

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = {
    val query =
      Query
        .newEntityQueryBuilder()
        .setKind(datastoreConfig.userWallet)
        .setFilter(PropertyFilter.eq("userId", userId.value))
        .build()
    Task(datastore.run(query, Seq.empty[ReadOption]: _*)).mapBoth({
      t: Throwable => WalletsFetchError(userId, t)
    }, results => results.asScala.toList.map(entityToWallet).collect { case Right(wallet) => wallet })
  }

  override def removeWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] =
    Task(datastore.delete(userWalletPk(userId, address)))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(UnableToRemoveWallet(address))
      .unit

  private def insertWallet(userId: UserId, address: WalletAddress)(timestamp: Instant): IO[WalletError, Unit] = {
    val entity = Entity
      .newBuilder(userWalletPk(userId, address))
      .set("userId", userId.value)
      .set("address", address.value)
      .set("addedAt", TimestampValue.of(Timestamp.ofTimeSecondsAndNanos(timestamp.getEpochSecond, timestamp.getNano)))
      .build()
    Task(datastore.put(entity))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(UnableToAddWallet(address))
      .unit
  }

  def getWallet(userId: UserId, address: WalletAddress): IO[WalletError, Wallet] =
    Task(datastore.get(userWalletPk(userId, address), Seq.empty[ReadOption]: _*)).mapError {
      case ex: DatastoreException => WalletFetchError(address, ex)
      case t: Throwable           => WalletFetchError(address, t)
    }.flatMap(entity => ZIO.fromOption(Option(entity)).orElseFail(WalletNotFound(userId, address)))
      .flatMap(entity => ZIO.fromEither(entityToWallet(entity)))
      .tapError(err => logger.warn(err.toString)) //TODO Maybe show??

  private val userWalletPk: (UserId, WalletAddress) => Key = (userId, address) =>
    datastore.newKeyFactory().setKind(datastoreConfig.userWallet).newKey(s"${userId.value}#${address.value}")

  private val entityToWallet: Entity => Either[InvalidWallet, Wallet] = entity => {
    for {
      userId         <- tryOrLeft(entity.getString("userId"), InvalidWallet("userId does not exist on entity"))
      address        <- tryOrLeft(entity.getString("address"), InvalidWallet("address does not exist on entity"))
      refinedUserId  <- refineV[NonEmpty](userId).mapLeft(_ => InvalidWallet("Invalid userId"))
      refinedAddress <- refineV[WalletAddressPredicate](address).mapLeft(_ => InvalidWallet("Invalid address"))
    } yield Wallet(refinedUserId, refinedAddress)
  }
}

object DatastoreUserWalletRepo {
  lazy val layer: URLayer[Has[Datastore] with Has[DatastoreConfig] with Logging with Clock, Has[UserWalletRepo]] =
    (DatastoreUserWalletRepo(_, _, _, _)).toLayer
}
