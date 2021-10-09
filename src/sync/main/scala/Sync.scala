package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.blockchain.BlockchainRepo
import domain.model.{ TransactionHash, WalletAddress }
import domain.wallet.WalletImportRepo
import domain.wallet.model.ImportDone
import infrastructure.binance.BnbListener
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore.DatastoreWalletImportRepo

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{ Config, ConfigFactory }
import org.web3j.protocol.core.methods.response.EthBlock.TransactionObject
import org.web3j.protocol.core.methods.response.Transaction
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._

object Sync extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) =
    for {
      bnbStream <- BnbListener
                    .positionEntryStream()
                    .map(_.getBlock.getTransactions.asScala.toList.map(_.get().asInstanceOf[TransactionObject]))
                    .flattenIterables
                    .filterM(txObject => filterByWalletPresence(txObject.get()))
                    .map(txObject => TransactionHash.unsafeApply(txObject.get().getHash))
                    .mapM(txHash => BlockchainRepo.fetchTransaction(txHash))
                    .foreach(t => UIO(println(t)))
                    .provideCustomLayer(listenerEnvironment(config))
                    .forever
                    .fork
      _ <- bnbStream.join
    } yield ()

  //TODO I think i need to split based on to vs from presence.
  private def filterByWalletPresence(tx: Transaction) =
    WalletImportRepo.getByImportStatus(ImportDone).flatMap { addresses =>
      if (tx.getTo != null && tx.getFrom != null) {
        val to           = WalletAddress.unsafeApply(tx.getTo)
        val from         = WalletAddress.unsafeApply(tx.getFrom)
        val addressFound = addresses.contains(to) || addresses.contains(from)
        UIO(addressFound)
      } else {
        UIO(false)
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
    lazy val walletRepoLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    exchangeRepoLayer ++ loggingLayer ++ walletRepoLayer
  }
}
