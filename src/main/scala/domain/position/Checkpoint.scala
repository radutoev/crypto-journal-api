package io.softwarechain.cryptojournal
package domain.position

import domain.model.WalletAddress

import java.time.Instant

final case class Checkpoint(address: WalletAddress, timestamp: Instant, latestTxTimestamp: Instant)
