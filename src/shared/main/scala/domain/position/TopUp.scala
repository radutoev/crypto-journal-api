package io.softwarechain.cryptojournal
package domain.position

import domain.model.{ Currency, Fee, FungibleData }
import domain.position.Position.PositionId

import java.time.Instant

//TODO Change PositionId to PlayId.
final case class TopUp(timestamp: Instant, value: FungibleData, fee: Fee, id: Option[PositionId] = None)
    extends MarketPlay
