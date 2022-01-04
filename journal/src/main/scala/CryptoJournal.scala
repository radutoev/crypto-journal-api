package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.market.LiveMarketService
import domain.portfolio.{LiveAccountBalance, LiveStatsService}
import domain.position.{LiveJournalingService, LiveMarketPlayService}
import infrastructure.api.Routes
import infrastructure.bitquery.BitQueryFacade
import infrastructure.coinapi.CoinApiFacadeHistoricalData
import infrastructure.covalent.CovalentFacade
import infrastructure.google.datastore._
import infrastructure.journal.service.LiveWalletService
import infrastructure.pricequote.LivePriceQuoteService
import infrastructure.sync.SyncFacade

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{Config, ConfigFactory}
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zhttp.service.server.ServerChannelFactory
import zhttp.service.{EventLoopGroup, Server}
import zio.clock.Clock
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger
import zio.{App, ExitCode, Has, URIO, ZIO, console}

object CryptoJournal extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) =
    (Server.port(8080) ++ Server.app(Routes.api)).make
      .use(_ => console.putStrLn("Server started on port 8080") *> ZIO.never)
      .provideCustomLayer(prepareEnvironment(config))

  private def prepareEnvironment(config: Config) = {
    val configLayer = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)

    val covalentConfigLayer  = configLayer.map(c => Has(c.get.covalent))
    val coinApiConfigLayer   = configLayer.map(c => Has(c.get.coinApi))
    val datastoreConfigLayer = configLayer.map(c => Has(c.get.datastore))
    val bitQueryConfigLayer  = configLayer.map(c => Has(c.get.bitquery))
    val syncConfigLayer      = configLayer.map(c => Has(c.get.sync))

    lazy val zioHttpServerLayer = EventLoopGroup.auto() ++ ServerChannelFactory.auto

    val datastoreLayer = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val coinApiFacadeLayer =
      (httpClientLayer ++ coinApiConfigLayer ++ loggingLayer) >+> CoinApiFacadeHistoricalData.layer

    lazy val covalentFacadeLayer = (httpClientLayer ++ covalentConfigLayer ++ loggingLayer) >>> CovalentFacade.layer

    lazy val bitQueryFacadeLayer = loggingLayer ++ bitQueryConfigLayer >>> BitQueryFacade.layer

    lazy val priceQuoteRepoLayer =
      datastoreLayer ++ datastoreConfigLayer ++ httpClientLayer ++ covalentConfigLayer ++ Clock.live ++ loggingLayer >>> DatastorePriceQuoteRepo.layer

    lazy val priceQuoteServiceLayer =
      bitQueryFacadeLayer ++ priceQuoteRepoLayer ++ loggingLayer >>> LivePriceQuoteService.layer

    lazy val userWalletRepo =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ Clock.live >>> DatastoreUserWalletRepo.layer

    lazy val walletImportLayer =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    lazy val paginationContextRepo =
      loggingLayer ++ datastoreLayer ++ Clock.live ++ datastoreConfigLayer >>> DatastorePaginationRepo.layer

    lazy val marketPlayRepo =
      datastoreLayer ++ datastoreConfigLayer ++ paginationContextRepo ++ loggingLayer ++ Clock.live >>> DatastoreMarketPlayRepo.layer

    lazy val journalRepoLayer = datastoreLayer ++ datastoreConfigLayer ++ loggingLayer >>> DatastoreJournalingRepo.layer

    lazy val marketPlayCacheLayer = priceQuoteRepoLayer >+> LiveMarketPlayService.cacheLayer

    lazy val accountBalanceLayer = priceQuoteServiceLayer ++ loggingLayer >>> LiveAccountBalance.layer

    lazy val marketPlayService =
      marketPlayRepo ++ marketPlayCacheLayer ++ priceQuoteServiceLayer ++ covalentFacadeLayer ++ journalRepoLayer ++ loggingLayer >>> LiveMarketPlayService.layer

    lazy val syncFacadeLayer = httpClientLayer ++ syncConfigLayer ++ loggingLayer >>> SyncFacade.layer

    lazy val walletServiceLayer =
      userWalletRepo ++ walletImportLayer ++ marketPlayService ++ syncFacadeLayer ++ loggingLayer >>> LiveWalletService.layer

    lazy val kpiServiceLayer = (marketPlayService ++ accountBalanceLayer ++ Clock.live ++ loggingLayer) >>> LiveStatsService.layer

    lazy val journalServiceLayer = journalRepoLayer >>> LiveJournalingService.layer

    lazy val marketServiceLayer = coinApiFacadeLayer >>> LiveMarketService.layer

    lazy val applicationServiceLayer =
      marketPlayService ++ walletServiceLayer ++ kpiServiceLayer ++ journalServiceLayer ++ marketServiceLayer

    zioHttpServerLayer ++ applicationServiceLayer ++ covalentFacadeLayer ++ priceQuoteRepoLayer ++ bitQueryFacadeLayer
  }
}
