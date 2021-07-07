package io.softwarechain.cryptojournal
package vo

import java.time.Instant

//TODO Add tests to check for interval validity.
final case class TimeInterval(start: Instant, end: Option[Instant] = None)
