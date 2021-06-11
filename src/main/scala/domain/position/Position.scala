package io.softwarechain.cryptojournal
package domain.position

import domain.model.State

import java.time.Instant

final case class Position(
  coin: String,
  state: State,
  openedAt: Instant,
  closedAt: Option[Instant],
  txHashes: List[String]
)
