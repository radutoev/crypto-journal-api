package io.softwarechain.cryptojournal
package application

import domain.position.Position
import service.PositionService

import zio.{Has, ZIO}

object CryptoJournalApi {
  def getPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[Position]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}
