package io.softwarechain.cryptojournal
package service

import domain.position.{CryptoFiatPosition, PositionRepo}

import zio.{Function1ToLayerSyntax, Has, Task, UIO, URLayer, ZIO}

trait PositionService {
  def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]]
}

object PositionService {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[CryptoFiatPosition]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}

final case class LivePositionService(positionRepo: PositionRepo) extends PositionService {
  override def getPositions(walletAddress: String): Task[List[CryptoFiatPosition]] = {
    for {
      positions <- positionRepo.getPositions(walletAddress)
      fiatEnriched <- UIO(List.empty) //TODO Implementation here
    } yield fiatEnriched
  }
}

object LivePositionService {
  lazy val layer: URLayer[Has[PositionRepo], Has[PositionService]] = (LivePositionService(_)).toLayer
}
