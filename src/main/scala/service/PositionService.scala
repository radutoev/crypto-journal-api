package io.softwarechain.cryptojournal
package service

import domain.model.FungibleData
import domain.position._
import domain.pricequote.PriceQuoteRepo
import vo.TimeInterval

import zio.{Function2ToLayerSyntax, Has, Task, URLayer, ZIO}

trait PositionService {
  def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]]

  def extractTimeInterval(positions: List[Position]): Option[TimeInterval] = {
    val timestamps = positions.flatMap(_.entries).map(_.timestamp)
    timestamps match {
      case head :: Nil => Some(TimeInterval(head, head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil => None
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
      positions <- positionRepo.getPositions(walletAddress)
      interval  = extractTimeInterval(positions)
      priceQuotes <- priceQuoteRepo.getQuotes(interval.get)
//
//
//      fiatEnriched <- ZIO.foreachPar(positions) { position =>
//                       val entries = position.entries
//                       for {
//                         feeTuples <- ZIO.foreach(entries)(entry =>
//                                       currencyService
//                                         .convert(entry.fee.amount, entry.timestamp)
//                                         .map(fiatValue =>
//                                           entry -> CryptoFiatFee(entry.fee, FungibleData(fiatValue, "USD"))
//                                         )
//                                     )
//                         entryToFiatFee = feeTuples.toMap
//                         valueTuples <- ZIO.foreach(entries)(entry =>
//                                         currencyService
//                                           .convert(entry.value.amount, entry.timestamp)
//                                           .map(fiatValue =>
//                                             entry -> CryptoFungibleData(entry.value, FungibleData(fiatValue, "USD"))
//                                           )
//                                       )
//                         entryToFiatValue = valueTuples.toMap
//                       } yield CryptoFiatPosition(
//                         position.coin,
//                         position.state,
//                         position.openedAt,
//                         position.closedAt,
//                         entries.map(entry =>
//                           CryptoFiatPositionEntry(
//                             entry.`type`,
//                             entryToFiatValue(entry),
//                             entryToFiatFee(entry),
//                             entry.timestamp
//                           )
//                         )
//                       )
//                     }
    } yield List.empty
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[PriceQuoteRepo], Has[PositionService]] =
    (LivePositionService(_, _)).toLayer
}
