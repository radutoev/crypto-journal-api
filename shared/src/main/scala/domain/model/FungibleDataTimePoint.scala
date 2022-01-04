package io.softwarechain.cryptojournal
package domain.model

import java.time.Instant

final case class FungibleDataTimePoint(
                                        fungibleData: FungibleData,
                                        timestamp: Instant,
                                        metadata: Option[FungibleDataTimePointMetadata] = None
                                      ) {
  override def toString: String =
    s"${fungibleData.amount},${fungibleData.currency},${timestamp}"
}

object FungibleDataTimePoint {
  def apply(fungibleData: FungibleData, timestamp: Instant): FungibleDataTimePoint =
    new FungibleDataTimePoint(fungibleData, timestamp)

  def apply(fungibleData: FungibleData, timestamp: Instant, metadata: FungibleDataTimePointMetadata): FungibleDataTimePoint =
    new FungibleDataTimePoint(fungibleData, timestamp, Some(metadata))
}

final case class FungibleDataTimePointMetadata(hash: TransactionHash, entryType: String)
