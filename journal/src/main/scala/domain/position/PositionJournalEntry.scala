package io.softwarechain.cryptojournal
package domain.position

import domain.model.PlayId

final case class PositionJournalEntry(positionId: PlayId, entry: JournalEntry)
