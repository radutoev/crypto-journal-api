package io.softwarechain.cryptojournal
package infrastructure.api.model

import domain.position.{Position => CryptoPosition}

import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.Instant

final case class Position(
  coin: String,
  state: String,
  openedAt: Instant,
  closedAt: Option[Instant],
  txHashes: List[String]
)

object Position {
  implicit val encoder: JsonCodec[Position] = DeriveJsonCodec.gen[Position]

  def fromPosition(position: CryptoPosition): Position = {
    Position(position.coin, position.state.toString, position.openedAt, position.closedAt, position.txHashes)
  }
}
