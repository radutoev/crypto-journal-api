package io.softwarechain.cryptojournal
package domain.position

import java.time.Instant

final case class JournalPositions(items: List[JournalPosition], lastSync: Option[Instant])
