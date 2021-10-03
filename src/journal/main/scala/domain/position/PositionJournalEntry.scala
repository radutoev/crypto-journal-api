package io.softwarechain.cryptojournal
package domain.position

import domain.position.Position.PositionId

final case class PositionJournalEntry(positionId: PositionId, entry: JournalEntry)
