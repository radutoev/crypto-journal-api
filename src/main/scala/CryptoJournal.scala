package io.softwarechain.cryptojournal

import domain.position.{ LiveJournalingService, LivePositionService }
import domain.portfolio.LiveKpiService
import domain.wallet.LiveWalletService
import infrastructure.api.Routes
import infrastructure.covalent.CovalentFacade
import infrastructure.google.{DatastoreJournalingRepo, DatastorePositionRepo, DatastoreWalletRepo, FirebasePriceQuoteRepo}

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

  def prepareEnvironment(config: Config) = {
    val configLayer = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)

    val covalentConfigLayer    = configLayer.map(c => Has(c.get.covalent))
    val demoAccountConfigLayer = configLayer.map(c => Has(c.get.demoAccount))

    lazy val zioHttpServerLayer = EventLoopGroup.auto() ++ ServerChannelFactory.auto

    val datastoreLayer = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val covalentFacadeLayer = ((httpClientLayer ++ covalentConfigLayer ++ loggingLayer) >>> CovalentFacade.layer)

    lazy val priceQuoteRepoLayer = datastoreLayer >>> FirebasePriceQuoteRepo.layer

    lazy val positionRepoLayer = datastoreLayer ++ loggingLayer ++ Clock.live >>> DatastorePositionRepo.layer

    lazy val journalRepoLayer = datastoreLayer ++ loggingLayer >>> DatastoreJournalingRepo.layer

    lazy val positionServiceLayer =
      positionRepoLayer ++ priceQuoteRepoLayer ++ covalentFacadeLayer ++ journalRepoLayer ++ demoAccountConfigLayer ++ loggingLayer >>> LivePositionService.layer

    lazy val walletServiceLayer =
      (loggingLayer ++ datastoreLayer ++ Clock.live >>> DatastoreWalletRepo.layer) ++ positionServiceLayer ++ loggingLayer >>> LiveWalletService.layer

    lazy val kpiServiceLayer = (positionServiceLayer ++ Clock.live) >>> LiveKpiService.layer

    lazy val journalServiceLayer = journalRepoLayer >>> LiveJournalingService.layer

    lazy val applicationServiceLayer = positionServiceLayer ++ walletServiceLayer ++ kpiServiceLayer ++ journalServiceLayer

    zioHttpServerLayer ++ applicationServiceLayer
  }
}
