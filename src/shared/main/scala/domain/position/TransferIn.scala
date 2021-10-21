package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Fee, FungibleData, TransactionHash }
import domain.position.Position.PositionId

import java.time.Instant

//TODO Change PositionId to PlayId.
final case class TransferIn(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  id: Option[PositionId] = None
) extends MarketPlay {
  override def openedAt: Instant = timestamp
}
