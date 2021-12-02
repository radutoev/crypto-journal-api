package io.softwarechain.cryptojournal

import application.SyncApi
import config.CryptoJournalConfig
import domain.model.WalletAddress
import domain.pricequote.LivePriceQuoteService
import domain.wallet.WalletImportRepo
import domain.wallet.model.ImportDone
import infrastructure.bitquery.BitQueryFacade
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore._

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{Config, ConfigFactory}
import org.web3j.protocol.core.methods.response.Transaction
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.clock.Clock
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger

object Sync extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) = {
    val (syncLayer, pagContextLayer) = listenerEnvironment(config)

    for {

      quotesSyncFiber <- SyncApi.updatePriceQuotes().provideCustomLayer(syncLayer).forever.fork
      pagContextFiber <- SyncApi.clearPaginationContext().provideCustomLayer(pagContextLayer).forever.fork
      _               <- quotesSyncFiber.join <+> pagContextFiber.join
      //      bnbStream <- BnbListener
      //                    .positionEntryStream()
      //                    .map(_.getBlock.getTransactions.asScala.toList.map(_.get().asInstanceOf[TransactionObject]))
      //                    .flattenIterables
      //                    .mapM(txObject =>
      //                      knownAddresses(txObject.get()).map(TransactionHash.unsafeApply(txObject.get().getHash) -> _)
      //                    )
      //                    .filter(_._2.nonEmpty)
      //                    .mapM {
      //                      case (txHash, addresses) =>
      //                        BlockchainRepo.fetchTransaction(txHash).map(_ -> addresses)
      //                    }
      //                    //TODO Redo logic for merging
      //                    //                    .mapM {
      //                    //                      case (transaction, addresses) =>
      //                    //                        ZIO.foreach(addresses)(address =>
      //                    //                          MarketPlayRepo
      //                    //                            .getLatestPosition(address, Currency.unsafeFrom(transaction.coin.get))
      //                    //                            .collect(MarketPlaysFetchError(address)) { case Some(position) => MarketPlays(List(position)) }
      //                    //                            .map(marketPlays => address -> marketPlays)
      //                    //                        )
      //                    //                    }
      //                    //                    .foreach { newAddressMarketPlays =>
      //                    //                      ZIO.foreach(newAddressMarketPlays) {
      //                    //                        case (address, marketPlays) =>
      //                    //                          MarketPlayRepo.save(address, marketPlays.positions)
      //                    //                      }
      //                    //                    }
      //                    .foreach(_ => UIO.unit)
      //                    .provideCustomLayer(listenerEnvironment(config))
      //                    .forever
      //                    .fork
      //      _ <- bnbStream.join
    } yield ()
  }

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
    val bitQueryConfigLayer  = configLayer.map(c => Has(c.get.bitquery))

    val datastoreLayer = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val clockLayer = Clock.live

    lazy val exchangeRepoLayer = (httpClientLayer ++ loggingLayer ++ covalentConfigLayer) >>> CovalentFacade.layer

    lazy val paginationContextRepoLayer =
      (loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ clockLayer) >>> DatastorePaginationRepo.layer

    lazy val marketPlayRepo =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ paginationContextRepoLayer >>> DatastoreMarketPlayRepo.layer

    lazy val walletRepoLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    lazy val currencyRepoLayer = datastoreLayer ++ datastoreConfigLayer ++ loggingLayer >>> DatastoreCurrencyRepo.layer

    lazy val priceQuoteRepoLayer =
      datastoreLayer ++ datastoreConfigLayer ++ clockLayer ++ loggingLayer >>> DatastorePriceQuoteRepo.layer

    lazy val bitQueryFacadeLayer = loggingLayer ++ bitQueryConfigLayer >>> BitQueryFacade.layer

    lazy val priceQuoteServiceLayer =
      (loggingLayer ++ priceQuoteRepoLayer ++ currencyRepoLayer ++ bitQueryFacadeLayer ++ clockLayer) >>> LivePriceQuoteService.layer

//    lazy val syncLayer =
//      exchangeRepoLayer ++ loggingLayer ++ walletRepoLayer ++ marketPlayRepo ++ currencyRepoLayer ++ priceQuoteServiceLayer ++ clockLayer ++ bitQueryFacadeLayer

    lazy val priceQuotesSyncLayer = priceQuoteServiceLayer

    (priceQuotesSyncLayer, paginationContextRepoLayer)
  }
}
