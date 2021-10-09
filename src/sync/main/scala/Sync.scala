package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.blockchain.BlockchainRepo
import domain.model.{ TransactionHash, WalletAddress }
import domain.position.{ PositionRepo, Positions }
import domain.wallet.WalletImportRepo
import domain.wallet.model.ImportDone
import infrastructure.binance.BnbListener
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore.{ DatastorePositionRepo, DatastoreWalletImportRepo }

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{ Config, ConfigFactory }
import org.web3j.protocol.core.methods.response.EthBlock.TransactionObject
import org.web3j.protocol.core.methods.response.Transaction
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.clock.Clock
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._

object Sync extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  //is it a buy or a sell?
  //fetch the transaction
  //group by address (from or to => tx together with type)
  private def program(config: Config) =
    for {
      bnbStream <- BnbListener
                    .positionEntryStream()
                    .map(_.getBlock.getTransactions.asScala.toList.map(_.get().asInstanceOf[TransactionObject]))
                    .flattenIterables
                    .mapM(txObject =>
                      knownAddresses(txObject.get()).map(TransactionHash.unsafeApply(txObject.get().getHash) -> _)
                    )
                    .filter(_._2.nonEmpty)
                    .mapM(hashAddressesTuple =>
                      BlockchainRepo.fetchTransaction(hashAddressesTuple._1).map(_ -> hashAddressesTuple._2)
                    )
                    //TODO Finish merging
                    .map {
                      case (transaction, addresses) =>
                        //for all addresses:
                        //get open position by coin and address
                        //
                        addresses.head -> Positions.empty()
                    }
                    .foreach { case (address, positions) => PositionRepo.save(address, positions.items) }
                    .provideCustomLayer(listenerEnvironment(config))
                    .forever
                    .fork
      _ <- bnbStream.join
    } yield ()

  private def knownAddresses(tx: Transaction) = {
    def elementOrNil(address: WalletAddress, addresses: List[WalletAddress]) =
      if (addresses.contains(address)) {
        List(address)
      } else {
        Nil
      }

    WalletImportRepo.getByImportStatus(ImportDone).flatMap { addresses =>
      if (tx.getTo != null && tx.getFrom != null) {
        val to             = WalletAddress.unsafeApply(tx.getTo)
        val from           = WalletAddress.unsafeApply(tx.getFrom)
        val foundAddresses = Nil ++ elementOrNil(to, addresses) ++ elementOrNil(from, addresses)
        UIO(foundAddresses)
      } else {
        UIO(Nil)
      }
    }
  }

  private def listenerEnvironment(config: Config) = {
    val configLayer          = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)
    val covalentConfigLayer  = configLayer.map(c => Has(c.get.covalent))
    val datastoreConfigLayer = configLayer.map(c => Has(c.get.datastore))

    val datastoreLayer = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val exchangeRepoLayer = (httpClientLayer ++ loggingLayer ++ covalentConfigLayer) >>> CovalentFacade.layer
    lazy val positionsRepoLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ Clock.live >>> DatastorePositionRepo.layer
    lazy val walletRepoLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    exchangeRepoLayer ++ loggingLayer ++ walletRepoLayer ++ positionsRepoLayer
  }
}
