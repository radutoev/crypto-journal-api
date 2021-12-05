package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.PriceQuotes
import util.ListOps.cond

import java.time.Instant

final case class TopUp(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  topUpDataGenerator: Option[TopUpData] = None,
  id: Option[PlayId] = None
) extends MarketPlay {

  override def openedAt: Instant = timestamp

  lazy val fees: Map[Currency, Fee] = topUpDataGenerator.map(_.fees(fee, timestamp)).getOrElse(Map.empty)

  lazy val balance: Option[FungibleData] = topUpDataGenerator.flatMap(_.balance(fee, value, timestamp))
}

object TopUp {
  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant): TopUp =
    new TopUp(txHash, value, fee, timestamp, None, None)

  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant, id: PlayId): TopUp =
    new TopUp(txHash, value, fee, timestamp, None, Some(id))
}
