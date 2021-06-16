package io.softwarechain.cryptojournal
package service

import domain.model.FungibleData
import domain.position._
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import vo.TimeInterval

import zio.{ Function2ToLayerSyntax, Has, Task, URLayer, ZIO }

trait PositionService {
  def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head, head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil          => None
    }
  }
}

object PositionService {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[CryptoFiatPosition]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}

final case class LivePositionService(positionRepo: PositionRepo, priceQuoteRepo: PriceQuoteRepo)
    extends PositionService {
  override def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]] =
    for {
      positions   <- positionRepo.getPositions(walletAddress)
      interval    = extractTimeInterval(positions)
      priceQuotes <- priceQuoteRepo.getQuotes(interval.get).map(PriceQuotes)
      fiatEnriched = positions.map { position =>
        CryptoFiatPosition(
          position.coin,
          position.state,
          position.openedAt,
          position.closedAt,
          enrichPositionEntries(position.entries)(priceQuotes)
        )
      }
    } yield fiatEnriched

  private def enrichPositionEntries(entries: List[PositionEntry])(implicit priceQuotes: PriceQuotes) =
    entries.map { entry =>
      val timestamp  = entry.timestamp
      val maybePrice = priceQuotes.findPrice(timestamp)
      if (maybePrice.isDefined) {
        val unitPrice = maybePrice.get.price
        CryptoFiatPositionEntry(
          `type` = entry.`type`,
          value = CryptoFungibleData(entry.value, Some(FungibleData(entry.value.amount * unitPrice, "USD"))),
          fee = CryptoFiatFee(entry.fee, Some(FungibleData(entry.fee.amount * unitPrice, "USD"))),
          timestamp = entry.timestamp
        )
      } else {
        CryptoFiatPositionEntry(
          `type` = entry.`type`,
          value = CryptoFungibleData(entry.value, None),
          fee = CryptoFiatFee(entry.fee, None),
          timestamp = entry.timestamp
        )
      }
    }
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo], Has[PositionService]] =
    (LivePositionService(_, _)).toLayer
}
