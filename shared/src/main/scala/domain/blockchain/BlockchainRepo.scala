package io.softwarechain.cryptojournal
package domain.blockchain

import domain.blockchain.error.TransactionsGetError
import domain.model.{TransactionHash, WalletAddress}

import zio.stream.ZStream
import zio.{Has, Task, ZIO}

import java.time.Instant

trait BlockchainRepo {
  def fetchTransactions(address: WalletAddress): Task[List[Transaction]]

  def transactionsStream(address: WalletAddress): ZStream[Any, TransactionsGetError, Transaction]

  def transactionsStream(address: WalletAddress, startFrom: Instant): ZStream[Any, TransactionsGetError, Transaction]

  def fetchTransaction(txHash: TransactionHash): Task[Transaction]
}

object BlockchainRepo {
  def fetchTransaction(txHash: TransactionHash): ZIO[Has[BlockchainRepo], Throwable, Transaction] =
    ZIO.accessM(_.get.fetchTransaction(txHash))
}
