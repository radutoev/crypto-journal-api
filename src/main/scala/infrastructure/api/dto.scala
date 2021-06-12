package io.softwarechain.cryptojournal
package infrastructure.api

import domain.position.{ Position => CJPosition, PositionEntry => CJPositionEntry }

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

  final case class PositionEntry(fee: Fee)

  final case class Fee(amount: BigDecimal, currency: String)

  object Position {
    implicit val feeCodec: JsonCodec[Fee]                     = DeriveJsonCodec.gen[Fee]
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
      PositionEntry(Fee(entry.fee.amount, entry.fee.currency))
  }
}
