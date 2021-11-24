package io.softwarechain.cryptojournal
package application

import domain.model.{Currency, WalletAddress}
import domain.position.{MarketPlayRepo, MarketPlays}

import zio.logging.{Logger, Logging}
import zio.{Has, ZIO}

object ReportsApi {
  def coinHistoryInWallet(address: WalletAddress, currency: Currency): ZIO[Has[MarketPlayRepo] with Logging, Throwable, Unit] = {
    ZIO.services[MarketPlayRepo, Logger[String]].flatMap { case (marketPlayRepo, logger) =>
      marketPlayRepo.getPlays(address)
        .map(MarketPlays(_))
        .map(_.currencyDistributionOverTime(currency))
        .mapBoth(
          _ => new RuntimeException("Generate report error"), {
          timePoints => {
            println(timePoints.map(_.fungibleData.amount).sum)
            timePoints.foreach(point => println(point.toString))
          }
      })
    }
  }
}
