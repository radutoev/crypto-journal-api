package io.softwarechain.cryptojournal
package vo

import domain.position.{ JournalEntry, Position }

final case class JournalPosition(position: Position, entry: Option[JournalEntry])
