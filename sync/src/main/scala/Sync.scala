package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.blockchain.BlockchainRepo
import domain.position.error.MarketPlaysFetchError
import domain.position.{ MarketPlayRepo, MarketPlays }
import domain.wallet.WalletImportRepo
import domain.wallet.model.ImportDone
import infrastructure.binance.BnbListener
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore.{ DatastoreMarketPlayRepo, DatastoreWalletImportRepo }

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{ Config, ConfigFactory }
import io.softwarechain.cryptojournal.domain.model.{ TransactionHash, WalletAddress }
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
                    .mapM {
                      case (txHash, addresses) =>
                        BlockchainRepo.fetchTransaction(txHash).map(_ -> addresses)
                    }
                    //TODO Redo logic for merging
                    //                    .mapM {
                    //                      case (transaction, addresses) =>
                    //                        ZIO.foreach(addresses)(address =>
                    //                          MarketPlayRepo
                    //                            .getLatestPosition(address, Currency.unsafeFrom(transaction.coin.get))
                    //                            .collect(MarketPlaysFetchError(address)) { case Some(position) => MarketPlays(List(position)) }
                    //                            .map(marketPlays => address -> marketPlays)
                    //                        )
                    //                    }
                    //                    .foreach { newAddressMarketPlays =>
                    //                      ZIO.foreach(newAddressMarketPlays) {
                    //                        case (address, marketPlays) =>
                    //                          MarketPlayRepo.save(address, marketPlays.positions)
                    //                      }
                    //                    }
                    .foreach(_ => UIO.unit)
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
        val to             = WalletAddress.unsafeFrom(tx.getTo)
        val from           = WalletAddress.unsafeFrom(tx.getFrom)
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
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ Clock.live >>> DatastoreMarketPlayRepo.layer
    lazy val walletRepoLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    exchangeRepoLayer ++ loggingLayer ++ walletRepoLayer ++ positionsRepoLayer
  }
}
