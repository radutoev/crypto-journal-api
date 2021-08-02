package io.softwarechain.cryptojournal
package domain.position

import domain.position.Position.PositionId

final case class TagPositions(tags: List[String], ids: List[PositionId])
