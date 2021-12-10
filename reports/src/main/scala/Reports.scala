package io.softwarechain.cryptojournal

import application.ReportsApi
import config.CryptoJournalConfig
import domain.model.{Currency, WalletAddress}
import infrastructure.google.datastore.{DatastoreMarketPlayRepo, DatastorePaginationRepo}

import com.google.cloud.datastore.DatastoreOptions
import com.typesafe.config.{Config, ConfigFactory}
import zio.clock.Clock
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger
import zio.{App, ExitCode, Has, URIO, ZIO}

object Reports extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) =
    ReportsApi
      .coinHistoryInWallet(
        WalletAddress.unsafeFrom("0x627909adab1ab107b59a22e7ddd15e5d9029bc41"),
        Currency.unsafeFrom("BUSD")
      )
      .provideCustomLayer(layer(config))

  private def layer(config: Config) = {
    val configLayer = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }
    val datastoreLayer       = ZIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer
    val datastoreConfigLayer = configLayer.map(c => Has(c.get.datastore))

    lazy val paginationContextRepoLayer =
      (loggingLayer ++ datastoreLayer ++ datastoreConfigLayer ++ Clock.live) >>> DatastorePaginationRepo.layer

    lazy val marketPlayRepo =
      datastoreLayer ++ datastoreConfigLayer ++ paginationContextRepoLayer ++ loggingLayer ++ Clock.live >>> DatastoreMarketPlayRepo.layer

    marketPlayRepo ++ loggingLayer
  }
}
