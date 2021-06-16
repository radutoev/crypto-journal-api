package io.softwarechain.cryptojournal
package infrastructure.google

import domain.pricequote.{PriceQuote, PriceQuoteRepo}
import util.CompletablePromise
import vo.TimeInterval

import com.google.cloud.Timestamp
import com.google.cloud.firestore.{Firestore, QueryDocumentSnapshot}
import zio.{Function1ToLayerSyntax, Has, Task, URLayer, ZIO}

import java.time.Instant
import scala.jdk.CollectionConverters._

final case class FirebasePriceQuoteRepo(firestore: Firestore) extends PriceQuoteRepo {
  override def getQuotes(interval: TimeInterval): Task[List[PriceQuote]] =
    for {
      querySnapshot <- ZIO.fromCompletionStage {
        new CompletablePromise(
          firestore.collection("price_quotes")
          //TODO Figure out how to apply filters.
//            .whereGreaterThanOrEqualTo("timestamp", LocalDateTime.ofInstant(interval.start, ZoneOffset.UTC))
//            .whereLessThanOrEqualTo("timestamp", LocalDateTime.ofInstant(interval.end, ZoneOffset.UTC))
            .get()
        )
      }
      priceQuotes   = querySnapshot.getDocuments.asScala.toList.map(snapshotToPriceQuote)
    } yield priceQuotes

  private val snapshotToPriceQuote: QueryDocumentSnapshot => PriceQuote = snapshot => {
    val dataMap = snapshot.getData.asScala.toMap
    val timestamp = Instant.ofEpochSecond(dataMap("timestamp").asInstanceOf[Timestamp].getSeconds)
    val price = dataMap("price").asInstanceOf[String].toFloat
    PriceQuote(price, timestamp)
  }
}

object FirebasePriceQuoteRepo {
  lazy val layer: URLayer[Has[Firestore], Has[PriceQuoteRepo]] = (FirebasePriceQuoteRepo(_)).toLayer
}
