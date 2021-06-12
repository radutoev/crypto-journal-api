package io.softwarechain.cryptojournal
package service

import domain.model.Fee
import domain.position.{ CryptoFiatFee, CryptoFiatPosition, CryptoFiatPositionEntry, PositionRepo }

import zio.{ Function2ToLayerSyntax, Has, Task, URLayer, ZIO }

trait PositionService {
  def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]]
}

object PositionService {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[CryptoFiatPosition]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}

final case class LivePositionService(positionRepo: PositionRepo, currencyService: CurrencyService)
    extends PositionService {
  override def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]] =
    for {
      positions <- positionRepo.getPositions(walletAddress)
      fiatEnriched <- ZIO.foreachPar(positions) { position =>
                       val entries = position.entries
                       for {
                         tuples <- ZIO.foreach(entries)(entry =>
                                    currencyService
                                      .convert(entry.fee.amount, entry.timestamp)
                                      .map(fiatValue => entry -> CryptoFiatFee(entry.fee, Fee(fiatValue, "USD")))
                                  )
                         entryToFiatFee = tuples.toMap
                       } yield CryptoFiatPosition(
                         position.coin,
                         position.state,
                         position.openedAt,
                         position.closedAt,
                         entries.map(entry =>
                           CryptoFiatPositionEntry(entry.`type`, entryToFiatFee(entry), entry.timestamp)
                         )
                       )
                     }
    } yield fiatEnriched
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo] with Has[CurrencyService], Has[PositionService]] =
    (LivePositionService(_, _)).toLayer
}
