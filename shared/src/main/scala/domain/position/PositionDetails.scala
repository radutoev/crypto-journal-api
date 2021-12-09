package io.softwarechain.cryptojournal
package domain.position

final case class PositionDetails[LinkRepresentation](position: Position, links: PositionLinks[LinkRepresentation])

final case class PositionLinks[LinkRepresentation](previous: List[LinkRepresentation], next: List[LinkRepresentation])
