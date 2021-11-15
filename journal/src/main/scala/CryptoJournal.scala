package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.market.LiveMarketService
import domain.portfolio.{LiveAccountBalance, LiveKpiService}
import domain.position.{LiveJournalingService, LiveMarketPlayService}
import domain.wallet.LiveWalletService
import infrastructure.api.Routes
import infrastructure.coinapi.CoinApiFacadeHistoricalData
import infrastructure.covalent.{CovalentFacade, CovalentWalletRepo}
import infrastructure.google.datastore._

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
//    val demoAccountConfigLayer = configLayer.map(c => Has(c.get.demoAccount))

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

    lazy val priceQuoteRepoLayer = datastoreLayer ++ datastoreConfigLayer ++ httpClientLayer ++ covalentConfigLayer ++ Clock.live ++ loggingLayer >>> DatastorePriceQuoteRepo.layer

    lazy val userWalletRepo =
      loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ Clock.live >>> DatastoreUserWalletRepo.layer

    lazy val walletImportLayer = loggingLayer ++ datastoreLayer ++ datastoreConfigLayer >>> DatastoreWalletImportRepo.layer

    lazy val positionRepoLayer =
      datastoreLayer ++ datastoreConfigLayer ++ loggingLayer ++ Clock.live >>> DatastoreMarketPlayRepo.layer

    lazy val journalRepoLayer = datastoreLayer ++ datastoreConfigLayer ++ loggingLayer >>> DatastoreJournalingRepo.layer

    lazy val walletRepoLayer = httpClientLayer ++ covalentConfigLayer ++ loggingLayer >>> CovalentWalletRepo.layer

    lazy val marketPlayService =
      positionRepoLayer ++ priceQuoteRepoLayer ++ covalentFacadeLayer ++ journalRepoLayer ++ loggingLayer >>> LiveMarketPlayService.layer

    lazy val walletServiceLayer =
      userWalletRepo ++ walletImportLayer ++ marketPlayService ++ loggingLayer >>> LiveWalletService.layer

    lazy val kpiServiceLayer = (marketPlayService ++ Clock.live ++ loggingLayer) >>> LiveKpiService.layer

    lazy val accountBalanceLayer = (marketPlayService ++ priceQuoteRepoLayer ++ walletRepoLayer ++ loggingLayer ++ Clock.live) >>> LiveAccountBalance.layer

    lazy val journalServiceLayer = journalRepoLayer >>> LiveJournalingService.layer

    lazy val marketServiceLayer = coinApiFacadeLayer >>> LiveMarketService.layer

    lazy val applicationServiceLayer =
      marketPlayService ++ walletServiceLayer ++ kpiServiceLayer ++ journalServiceLayer ++ marketServiceLayer ++ accountBalanceLayer

    zioHttpServerLayer ++ applicationServiceLayer ++ covalentFacadeLayer
  }
}
