package io.softwarechain.cryptojournal
package infrastructure.google

import domain.model.{UserId, WalletAddress, WalletAddressPredicate}
import domain.wallet.{Wallet, WalletRepo}
import domain.wallet.error._
import infrastructure.google.DatastoreWalletRepo.WalletKind
import util.EitherOps

import com.google.cloud.datastore.StructuredQuery.PropertyFilter
import com.google.cloud.datastore._
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import zio.logging.{Logger, Logging}
import zio.{Has, IO, Task, URLayer, ZIO}

import scala.util.Try
import scala.jdk.CollectionConverters._

final case class DatastoreWalletRepo(datastore: Datastore, logger: Logger[String]) extends WalletRepo {
  override def addWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    getWallet(userId, address)
      .zipRight(ZIO.fail(WalletAddressExists(address)))
      .catchSome {
        case _: WalletNotFound => insertWallet(userId, address)
      }
  }

  override def getWallets(userId: UserId): IO[WalletError, List[Wallet]] = {
    val query = Query.newEntityQueryBuilder().setKind(WalletKind).setFilter(PropertyFilter.eq("userId", userId.value)).build()
    Task(datastore.run(query, Seq.empty[ReadOption]: _*))
      .mapError {
        case t: Throwable => WalletsFetchError(userId, t)
      }
      .map(results => results.asScala.toList.map(entityToWallet).collect { case Right(wallet) => wallet })
  }

  private def insertWallet(userId: UserId, address: WalletAddress): IO[WalletError, Unit] = {
    val entity = Entity.newBuilder(userWalletPk(userId, address)).set("userId", userId.value).set("address", address.value).build()
    Task(datastore.put(entity))
      .tapError(err => logger.warn(err.toString))
      .orElseFail(UnableToAddWallet(address))
      .unit
  }

  def getWallet(userId: UserId, address: WalletAddress): IO[WalletError, Wallet] = {
    Task(datastore.get(userWalletPk(userId, address), Seq.empty[ReadOption]: _*))
      .mapError {
        case ex: DatastoreException => WalletFetchError(userId, address, ex)
        case t: Throwable => WalletFetchError(userId, address, t)
      }
      .flatMap(entity => ZIO.fromOption(Option(entity)).orElseFail(WalletNotFound(userId, address)))
      .flatMap(entity => ZIO.fromEither(entityToWallet(entity)))
      .tapError(err => logger.warn(err.toString)) //TODO Maybe show??
  }

  private val userWalletPk: (UserId, WalletAddress) => Key = (userId, address) =>
    datastore.newKeyFactory().setKind(WalletKind).newKey(s"${userId.value}#${address.value}")

  private val entityToWallet: Entity => Either[InvalidWallet, Wallet] = entity => {
    for {
      userId <- tryOrMessage(entity.getString("userId"), InvalidWallet("userId does not exist on entity"))
      address <- tryOrMessage(entity.getString("address"), InvalidWallet("address does not exist on entity"))
      refinedUserId <- refineV[NonEmpty](userId).mapLeft(_ => InvalidWallet("Invalid userId"))
      refinedAddress <- refineV[WalletAddressPredicate](address).mapLeft(_ => InvalidWallet("Invalid address"))
    } yield Wallet(refinedUserId, refinedAddress)
  }

  private def tryOrMessage[T, E](f: => T, fail: E): Either[E, T] = {
    Try(f).toEither.left.map(_ => fail)
  }
}

object DatastoreWalletRepo {
  lazy val layer: URLayer[Has[Datastore] with Logging, Has[WalletRepo]] = (DatastoreWalletRepo(_, _)).toLayer

  val WalletKind: String = "Wallet"
}
