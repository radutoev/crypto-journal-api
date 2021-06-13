package io.softwarechain.cryptojournal
package infrastructure.api

import domain.position.{ CryptoFiatPosition => CJPosition, CryptoFiatPositionEntry => CJPositionEntry }

import zio.json.{ DeriveJsonCodec, JsonCodec }

import java.time.Instant

object dto {
  final case class Position(
    coin: String,
    state: String,
    openedAt: Instant,
    closedAt: Option[Instant],
    entries: List[PositionEntry]
  )

  final case class PositionEntry(value: FungibleData, fiatValue: FungibleData, fee: FungibleData, fiatFee: FungibleData)

  final case class FungibleData(amount: BigDecimal, currency: String)

  object Position {
    implicit val feeCodec: JsonCodec[FungibleData]                     = DeriveJsonCodec.gen[FungibleData]
    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]

    def fromPosition(position: CJPosition): Position =
      Position(
        position.coin,
        position.state.toString,
        position.openedAt,
        position.closedAt,
        position.entries.map(fromPositionEntry)
      )

    def fromPositionEntry(entry: CJPositionEntry): PositionEntry =
      PositionEntry(
        FungibleData(entry.value.crypto.amount, entry.value.crypto.currency),
        FungibleData(entry.value.fiat.amount, entry.value.fiat.currency),
        FungibleData(entry.fee.crypto.amount, entry.fee.crypto.currency),
        FungibleData(entry.fee.fiat.amount, entry.fee.fiat.currency)
      )
  }
}
