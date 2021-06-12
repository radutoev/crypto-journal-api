package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Fee, State, TransactionType }

import java.time.Instant

final case class CryptoFiatPosition(
  coin: String,
  state: State,
  openedAt: Instant,
  closedAt: Option[Instant],
  entries: List[CryptoFiatPositionEntry]
)

final case class CryptoFiatPositionEntry(`type`: TransactionType, fee: CryptoFiatFee, timestamp: Instant)

final case class CryptoFiatFee(crypto: Fee, fiat: Fee)
