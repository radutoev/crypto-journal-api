package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{FungibleData => CJFungibleData}
import domain.position.{Position => CJPosition, PositionEntry => CJPositionEntry}
import domain.pricequote.PriceQuotes

import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.Instant

object dto {
  final case class Position(
    coin: String,
    state: String,
    openedAt: Instant,
    closedAt: Option[Instant],
    entries: List[PositionEntry]
  )

  final case class PositionEntry(
    `type`: String,
    value: FungibleData,
    fiatValue: Option[FungibleData],
    fee: FungibleData,
    fiatFee: Option[FungibleData]
  )

  final case class FungibleData(amount: BigDecimal, currency: String)

  object Position {
    implicit val feeCodec: JsonCodec[FungibleData]            = DeriveJsonCodec.gen[FungibleData]
    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]

    def fromPosition(position: CJPosition): Position =
      Position(
        position.coin,
        position.state.toString,
        position.openedAt,
        position.closedAt,
        position.entries.map(entry => fromPositionEntry(entry)(position.priceQuotes.getOrElse(PriceQuotes.empty())))
      )

    def fromPositionEntry(entry: CJPositionEntry)(implicit priceQuotes: PriceQuotes): PositionEntry = {
      PositionEntry(
        entry.`type`.toString,
        entry.value.asJson,
        entry.fiatValue.map(_.asJson),
        entry.fee.asJson,
        entry.fiatFee.map(_.asJson)
      )
    }
  }

  implicit class FungibleDataOps(data: CJFungibleData) {
    def asJson: FungibleData = FungibleData(data.amount, data.currency)
  }
}
