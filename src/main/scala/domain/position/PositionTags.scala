package io.softwarechain.cryptojournal
package domain.position

import domain.position.Position.PositionId

final case class PositionTags(positionId: PositionId, tags: List[String])
