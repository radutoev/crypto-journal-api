package io.softwarechain.cryptojournal

import application.SyncApi
import config.CryptoJournalConfig
import domain.model.WalletAddress
import domain.pricequote.LivePriceQuotesJobService
import domain.wallet.{LocalWalletCache, WalletCache, WalletRepo}
import domain.wallet.model.ImportDone
import infrastructure.bitquery.BitQueryFacade
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore._

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{Config, ConfigFactory}
import io.softwarechain.cryptojournal.infrastructure.api.Routes
import org.web3j.protocol.core.methods.response.Transaction
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zhttp.service.{EventLoopGroup, Server}
import zhttp.service.server.ServerChannelFactory
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
    val (priceQuoteLayer, pagContextLayer, syncLayer, zioHttpLayer) = listenerEnvironment(config)

    for {
      quotesSyncFiber <- SyncApi.updatePriceQuotes().provideCustomLayer(priceQuoteLayer).fork
      pagContextFiber <- SyncApi.clearPaginationContext().provideCustomLayer(pagContextLayer).fork
//      syncFiber       <- (SyncApi.loadWallets() *> SyncApi.updatePositions()).provideCustomLayer(syncLayer).fork
      httpFiber       <- (Server.port(8080) ++ Server.app(Routes.api))
        .make
        .use(_ => console.putStrLn("Server started on port 8080") *> ZIO.never)
        .provideCustomLayer(zioHttpLayer)
        .fork
      _               <- (quotesSyncFiber <*>  pagContextFiber /*<*> syncFiber */ <*> httpFiber).join
    } yield ()
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

    lazy val walletCacheLayer = LocalWalletCache.layer

    lazy val priceQuoteRepoLayer =
      datastoreLayer ++ datastoreConfigLayer ++ clockLayer ++ loggingLayer >>> DatastorePriceQuoteRepo.layer

    lazy val bitQueryFacadeLayer = loggingLayer ++ bitQueryConfigLayer >>> BitQueryFacade.layer

    lazy val priceQuoteServiceLayer =
      (loggingLayer ++ priceQuoteRepoLayer ++ bitQueryFacadeLayer ++ clockLayer) >>> LivePriceQuotesJobService.layer

    lazy val syncLayer =
      exchangeRepoLayer ++ loggingLayer ++ walletCacheLayer ++ marketPlayRepo ++ walletRepoLayer

    lazy val priceQuotesSyncLayer = priceQuoteServiceLayer

    lazy val zioHttpServerLayer = EventLoopGroup.auto() ++ ServerChannelFactory.auto

    (priceQuotesSyncLayer, paginationContextRepoLayer, syncLayer, zioHttpServerLayer ++ walletCacheLayer)
  }
}
