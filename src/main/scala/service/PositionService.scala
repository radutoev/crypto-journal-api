package io.softwarechain.cryptojournal
package service

import domain.position._
import domain.pricequote.{ PriceQuoteRepo, PriceQuotes }
import vo.TimeInterval

import zio.{ Function2ToLayerSyntax, Has, Task, URLayer, ZIO }

trait PositionService {
  def getPositions(walletAddress: String): Task[List[Position]]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head, None))
      case head :: tail => Some(TimeInterval(head, Some(tail.last)))
      case Nil          => None
    }
  }
}

object PositionService {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[Position]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}

final case class LivePositionService(positionRepo: PositionRepo, priceQuoteRepo: PriceQuoteRepo)
    extends PositionService {
  override def getPositions(walletAddress: String): Task[List[Position]] =
    for {
      positions   <- positionRepo.getPositions(walletAddress)
      interval    = extractTimeInterval(positions)
      priceQuotes <- priceQuoteRepo.getQuotes(interval.get).map(PriceQuotes.apply)
      enrichedPositions = positions.map(position => position.copy(priceQuotes = Some(priceQuotes))) //.subset(position.timeInterval())
    } yield enrichedPositions
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo], Has[PositionService]] =
    (LivePositionService(_, _)).toLayer
}
