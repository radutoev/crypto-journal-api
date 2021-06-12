package io.softwarechain.cryptojournal
package application

import domain.position.CryptoFiatPosition
import service.PositionService

import zio.{Has, ZIO}

object CryptoJournalService {
  def getCryptoFiatPositions(walletAddress: String): ZIO[Has[PositionService], Throwable, List[CryptoFiatPosition]] =
    ZIO.serviceWith[PositionService](_.getPositions(walletAddress))
}
