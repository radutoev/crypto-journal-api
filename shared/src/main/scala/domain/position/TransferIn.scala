package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Fee, FungibleData, PlayId, TransactionHash }
import domain.pricequote.PriceQuotes

import eu.timepit.refined
import eu.timepit.refined.collection.NonEmpty

import java.time.Instant

//TODO Should I handle PriceQuotes differently?? Maybe as an implicit on the functions??
final case class TransferIn(
  txHash: TransactionHash,
  value: FungibleData,
  fee: Fee,
  timestamp: Instant,
  id: Option[PlayId] = None,
  priceQuotes: Option[PriceQuotes] = None
) extends MarketPlay {

  override def openedAt: Instant = timestamp

  override def fiatValue(): Option[FungibleData] = priceQuotes.flatMap { quotes =>
    quotes
      .findPrice(timestamp)
      .map(quote => value.amount * quote.price)
      .map(fiatAmount => FungibleData(fiatAmount, refined.refineV[NonEmpty].unsafeFrom("USD")))
  }

  override def totalFees(): Option[Fee] = priceQuotes.flatMap { quotes =>
    quotes
      .findPrice(timestamp)
      .map(quote => fee.amount * quote.price)
      .map(fiatFee => FungibleData(fiatFee, refined.refineV[NonEmpty].unsafeFrom("USD")))
  }
}
