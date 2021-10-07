package io.softwarechain.cryptojournal
package infrastructure.google.pubsub

import domain.wallet.WalletMessaging
import domain.wallet.error.{ WalletError, WalletMessagingError }
import domain.wallet.event.WalletEvent
import infrastructure.google.pubsub.json._

import com.google.cloud.pubsublite.cloudpubsub.Publisher
import com.google.protobuf.ByteString
import com.google.pubsub.v1.PubsubMessage
import com.spotify.futures.ApiFuturesExtra
import zio.json._
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, URLayer, ZIO }

import java.nio.charset.Charset

final case class PubSubWalletMessaging(publisher: Publisher, logger: Logger[String]) extends WalletMessaging {
  override def publish(event: WalletEvent): IO[WalletError, Unit] = {
    val message =
      PubsubMessage.newBuilder().setData(ByteString.copyFrom(event.toJson, Charset.forName("UTF-8"))).build()
    ZIO
      .fromCompletionStage(ApiFuturesExtra.toCompletableFuture(publisher.publish(message)))
      .tapError(throwable => logger.warn(s"Unable to publish event ${event.toJson}. Reason: ${throwable.getMessage}"))
      .mapError(throwable => WalletMessagingError(throwable))
      .unit
  }
}

object PubSubWalletMessaging {
  lazy val layer: URLayer[Has[Publisher] with Logging, Has[WalletMessaging]] = (PubSubWalletMessaging(_, _)).toLayer
}
