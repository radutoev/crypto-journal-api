package io.softwarechain.cryptojournal
package application

import domain.blockchain.BlockchainRepo
import domain.model.{TransactionHash, WalletAddress}
import domain.position.PositionEntry
import domain.pricequote.{CurrencyPair, PriceQuote, PriceQuoteRepo}
import vo.TimeInterval

import zio.{Has, UIO, ZIO}

object PositionHelper {
  def txToEntries(
    address: WalletAddress,
    txHash: TransactionHash
  ): ZIO[Has[BlockchainRepo], Throwable, List[PositionEntry]] =
    ZIO.serviceWith(
      _.fetchTransaction(txHash)
        .flatMap(tx => ZIO.fromEither(PositionEntry.fromTransaction(tx, address))
          .tapError(err => UIO(println(err)))
          .mapError(new RuntimeException(_)))
    )

  def quotes(pair: CurrencyPair, interval: TimeInterval): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] = {
    ZIO.serviceWith(
      _.getQuotes(pair, interval)
        .tapError(err => UIO(println(err)))
        .mapError(err => new RuntimeException(err.toString))
    )
  }
}
