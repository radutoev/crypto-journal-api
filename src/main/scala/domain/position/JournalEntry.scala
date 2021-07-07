package io.softwarechain.cryptojournal
package domain.position

import eu.timepit.refined.types.string.NonEmptyString

final case class JournalEntry(notes: Option[NonEmptyString])
