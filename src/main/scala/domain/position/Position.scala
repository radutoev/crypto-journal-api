package io.softwarechain.cryptojournal
package domain.position

import Position.{ PositionEntryId, PositionId }
import domain.model._
import domain.pricequote.{ PriceQuote, PriceQuotes }
import vo.TimeInterval

import eu.timepit.refined
import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

import java.time.{ Duration, Instant }

final case class Position(
  currency: Currency,
  openedAt: Instant,
  entries: List[PositionEntry],
  priceQuotes: Option[PriceQuotes] = None, //this is kind of a meta information for the aggregate.
  id: Option[PositionId] = None
) {
  def timeInterval(): TimeInterval = TimeInterval(openedAt, closedAt)

  /**
   * Total cost is calculated from all BUY entries and all fees within in the position.
   * It is an absolute value.
   * @return
   */
  def totalCost(): Option[FungibleData] =
    priceQuotes.map { implicit quotes =>
      val buyCost  = entries.filter(_.isBuy()).map(_.fiatTotal()).sumFungibleData()
      val sellCost = entries.filter(_.isSell()).map(_.fiatFee()).sumFungibleData()
      FungibleData(buyCost.add(sellCost.amount).amount.abs, refined.refineV[NonEmpty].unsafeFrom("USD"))
    }

  /**
   * @return Fiat sum of all fees for all entries in this position
   */
  def totalFees(): Option[FungibleData] =
    priceQuotes.map(implicit quotes => entries.map(_.fiatFee()).sumFungibleData())

  /**
   * Total fiat value of position.
   * Basically what is the fiat value of the coins in the position.
   */
  def fiatValue(): Option[FungibleData] =
    priceQuotes.map { implicit quotes =>
      entries.collect {
        case e: PositionEntry if e.fiatValue().isDefined => {
          if (e.isBuy()) e.fiatValue().get
          else e.fiatValue().get.negate()
        }
      }.sumFungibleData()
    }

  /**
   * Position return derived from all position entries associated with this position.
   * @return
   * None if position is not closed or no price quotes are given for the position interval.
   * FungibleData for a closed position.
   */
  def fiatReturn(): Option[FungibleData] =
    if (state == Open) {
      None
    } else {
      priceQuotes.map { implicit quotes =>
        entries
          .map(_.fiatTotal())
          .sumFungibleData()
      }
    }

  /**
   * @return Total number of coins that were bought within this position
   */
  def totalCoins(): FungibleData =
    FungibleData(
      entries.filter(_.isBuy()).map(_.value.amount).sum,
      currency
    )

  /**
   * @return Entry coin fiat price
   */
  def entryPrice(): Option[PriceQuote] =
    priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.head.timestamp))

  /**
   * @return Exit coin fiat price
   */
  def exitPrice(): Option[PriceQuote] =
    if (closedAt.isDefined) {
      priceQuotes.flatMap(implicit quotes => quotes.findPrice(entries.last.timestamp))
    } else {
      None
    }

  def numberOfExecutions(): Int = entries.size

  def holdTime: Option[Long] = closedAt.map(closeTime => Duration.between(openedAt, closeTime).toSeconds)

  def win(): Option[Boolean] = fiatReturn().map(_.amount.compareTo(BigDecimal(0)) > 0)

  def state: State = entries.lastOption.fold[State](Open)(last => if (last.isSell()) Closed else Open)

  def isClosed(): Boolean = state == Closed

  def isOpen(): Boolean = state == Open

  def closedAt(): Option[Instant] = entries.lastOption.collect {
    case entry if entry.`type` == Sell => entry.timestamp
  }
}

object Position {
  type PositionIdPredicate = NonEmpty
  type PositionId          = String Refined PositionIdPredicate

  type PositionEntryIdPredicate = NonEmpty
  type PositionEntryId          = String Refined PositionEntryIdPredicate
}

final case class PositionEntry(
  `type`: TransactionType,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  txHash: TransactionHash,
  id: Option[PositionEntryId] = None
) {
  def isBuy(): Boolean = `type` == Buy

  def isSell(): Boolean = `type` == Sell

  def fiatValue()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
    priceQuotes
      .findPrice(timestamp)
      .map(priceQuote => value.amount * priceQuote.price)
      .map(fiatAmount => FungibleData(fiatAmount, refined.refineV[NonEmpty].unsafeFrom("USD")))

  def fiatFee()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
    priceQuotes
      .findPrice(timestamp)
      .map(priceQuote => fee.amount * priceQuote.price)
      .map(fiatAmount => FungibleData(fiatAmount, refined.refineV[NonEmpty].unsafeFrom("USD")))

  /**
   * Calculates fiat value for the given price quotes based on the type of the entry.
   * If entry is of type BUY, then it will return the negative absolute number of the fiatValue and fiatFee values.
   * If entry is of type SELL, then it will subtract the fiatFee from the fiatValue.
   */
  def fiatTotal()(implicit priceQuotes: PriceQuotes): Option[FungibleData] =
    for {
      fiatValue <- fiatValue()
      fiatFee   <- fiatFee()
      fiatReturn <- Some[BigDecimal](`type` match {
                     case Buy  => -fiatValue.amount - fiatFee.amount
                     case Sell => fiatValue.amount - fiatFee.amount
                     case _    => 0
                   })
    } yield FungibleData(fiatReturn, refined.refineV[NonEmpty].unsafeFrom("USD"))
}
