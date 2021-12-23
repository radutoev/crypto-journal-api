package io.softwarechain.cryptojournal
package infrastructure.sync

import config.SyncConfig
import domain.model.WalletAddress

import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.{ Has, Task, URLayer, ZIO }
import zio.logging.{ Logger, Logging }

final case class SyncFacade(httpClient: SttpClient.Service, config: SyncConfig, logger: Logger[String]) {
  def addWallet(walletAddress: WalletAddress): Task[Unit] = {
    val url = s"${config.url}/wallets/${walletAddress.value}"
    httpClient
      .send(basicRequest.put(uri"$url").response(asString))
      .tapError(throwable => logger.warn(throwable.getMessage))
      .map(_.code.isSuccess)
      .flatMap(success => ZIO.fail(new RuntimeException("Error adding wallet")).unless(success))
  }
}

object SyncFacade {
  lazy val layer: URLayer[SttpClient with Has[SyncConfig] with Logging, Has[SyncFacade]] =
    (SyncFacade(_, _, _)).toLayer
}
