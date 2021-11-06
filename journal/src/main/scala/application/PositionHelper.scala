package io.softwarechain.cryptojournal
package application

import domain.blockchain.BlockchainRepo
import domain.model.{ TransactionHash, WalletAddress }
import domain.position.PositionEntry

import zio.{ Has, ZIO }

object PositionHelper {
  def txToEntries(
    address: WalletAddress,
    txHash: TransactionHash
  ): ZIO[Has[BlockchainRepo], Throwable, List[PositionEntry]] =
    ZIO.serviceWith(
      _.fetchTransaction(txHash)
        .flatMap(tx => ZIO.fromEither(PositionEntry.fromTransaction(tx, address)).mapError(new RuntimeException(_)))
    )
}
