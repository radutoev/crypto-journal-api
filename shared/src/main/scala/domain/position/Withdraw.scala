package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.pricequote.PriceQuotes
import util.ListOps.cond

import java.time.Instant

final case class Withdraw(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  withdrawDataGenerator: Option[WithdrawData],
  id: Option[PlayId] = None
) extends MarketPlay {
  override def openedAt: Instant = timestamp

  lazy val fees: Map[Currency, Fee] = withdrawDataGenerator.map(_.fees(fee, timestamp)).getOrElse(Map.empty)

  //hardcoded to USD for now
  lazy val balance: Option[FungibleData] = withdrawDataGenerator.flatMap(_.balance(fee, value, timestamp))
}

object Withdraw {
  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant): Withdraw =
    new Withdraw(txHash, value, fee, timestamp, None)

  def apply(txHash: TransactionHash, value: FungibleData, fee: Fee, timestamp: Instant, id: PlayId): Withdraw =
    new Withdraw(txHash, value, fee, timestamp, None, Some(id))
}
