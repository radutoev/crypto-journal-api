package io.softwarechain.cryptojournal

import domain.position.LivePositionRepo
import infrastructure.api.Routes
import infrastructure.covalent.CovalentFacade
import infrastructure.google.FirebasePriceQuoteRepo
import service.LivePositionService

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{ Config, ConfigFactory }
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zhttp.service.server.ServerChannelFactory
import zhttp.service.{ EventLoopGroup, Server }
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger
import zio.{ console, App, ExitCode, Has, Task, URIO, ZIO, ZLayer }

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

    val covalentConfigLayer = configLayer.map(c => Has(c.get.covalent))

    lazy val zioHttpServerLayer = EventLoopGroup.auto() ++ ServerChannelFactory.auto

    val datastoreLayer = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val priceQuoteRepoLayer = datastoreLayer >>> FirebasePriceQuoteRepo.layer

    lazy val positionRepoLayer =
      ((httpClientLayer ++ covalentConfigLayer ++ loggingLayer) >>> CovalentFacade.layer) >>> LivePositionRepo.layer

    lazy val positionServiceLayer = positionRepoLayer ++ priceQuoteRepoLayer >>> LivePositionService.layer

    lazy val applicationServiceLayer = positionServiceLayer

    zioHttpServerLayer ++ applicationServiceLayer
  }
}
