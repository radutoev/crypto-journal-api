package io.softwarechain.cryptojournal

import domain.position.LivePositionRepo
import infrastructure.api.Routes
import infrastructure.covalent.CovalentFacade

import com.typesafe.config.{Config, ConfigFactory}
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zhttp.service.server.ServerChannelFactory
import zhttp.service.{EventLoopGroup, Server}
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger
import zio.{App, ExitCode, Has, URIO, ZIO, console}

object CryptoJournal extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) = {
    (Server.port(8080) ++ Server.app(Routes.api))
      .make
      .use(_ => console.putStrLn("Server started on port 8080") *> ZIO.never)
      .provideCustomLayer(prepareEnvironment(config))
  }

  private def prepareEnvironment(config: Config) = {
    val configLayer = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)

    val covalentConfigLayer = configLayer.map(c => Has(c.get.covalent))

    lazy val zioHttpServerLayer = EventLoopGroup.auto() ++ ServerChannelFactory.auto
//    lazy val zioHttpClientLayer = EventLoopGroup.auto() ++ ChannelFactory.auto

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make { (_, message) =>
        logFormat.format(message)
      }
    }

//    lazy val httpClientLayer = Client.make.provideLayer(zioHttpClientLayer).toLayer
    lazy val httpClientLayer = HttpClientZioBackend.layer()

    zioHttpServerLayer ++ (((httpClientLayer ++ covalentConfigLayer ++ loggingLayer) >>> CovalentFacade.layer) >>> LivePositionRepo.layer)
  }
}
