package io.softwarechain.cryptojournal
package domain.position

import domain.model.WalletAddress

import java.time.Instant

//I use the latestTxTimestamp for now as a control mechanism of determining whether a not an import job is currently running.
//This works for now as the use case is not complex, but this might need refactoring if requirements change.
final case class Checkpoint(address: WalletAddress, timestamp: Instant, latestTxTimestamp: Option[Instant])
