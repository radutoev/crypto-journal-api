package io.softwarechain.cryptojournal
package domain.blockchain

import domain.blockchain.error.{ BlockchainError, TransactionsGetError }
import domain.model.{ CoinAddress, TransactionHash, WalletAddress }
import domain.pricequote.PriceQuote
import vo.TimeInterval

import zio.stream.ZStream
import zio.{ Has, IO, Task, ZIO }

import java.time.Instant

trait BlockchainRepo {
  def fetchTransactions(address: WalletAddress): Task[List[Transaction]]

  def transactionsStream(address: WalletAddress): ZStream[Any, TransactionsGetError, Transaction]

  def transactionsStream(address: WalletAddress, startFrom: Instant): ZStream[Any, TransactionsGetError, Transaction]

  def fetchTransaction(txHash: TransactionHash): Task[Transaction]

  def getHistoricalPriceQuotes(coinAddress: CoinAddress, interval: TimeInterval): IO[BlockchainError, List[PriceQuote]]
}

object BlockchainRepo {
  def fetchTransaction(txHash: TransactionHash): ZIO[Has[BlockchainRepo], Throwable, Transaction] =
    ZIO.accessM(_.get.fetchTransaction(txHash))
}
