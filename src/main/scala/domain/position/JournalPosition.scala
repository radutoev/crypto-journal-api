package io.softwarechain.cryptojournal
package domain.position

final case class JournalPosition(position: Position, entry: Option[JournalEntry])
