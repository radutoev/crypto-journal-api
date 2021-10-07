package io.softwarechain.cryptojournal
package infrastructure.google.pubsub

import domain.wallet.{ error, event, WalletMessaging }

import com.google.cloud.pubsublite.cloudpubsub.Publisher
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, URLayer }

final case class PubSubWalletMessaging(publisher: Publisher, logger: Logger[String]) extends WalletMessaging {
  override def publish(event: event.WalletEvent): IO[error.WalletError, Unit] = ???
}

object PubSubWalletMessaging {
  lazy val layer: URLayer[Has[Publisher] with Logging, Has[WalletMessaging]] = (PubSubWalletMessaging(_, _)).toLayer
}
