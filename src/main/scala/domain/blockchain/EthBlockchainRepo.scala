package io.softwarechain.cryptojournal
package domain.blockchain

import domain.blockchain.error.TransactionsGetError
import domain.model.WalletAddress

import zio.stream.ZStream
import zio.{ Has, Task, ZIO }

import java.time.Instant

trait EthBlockchainRepo {
  def fetchTransactions(address: WalletAddress): Task[List[Transaction]]

  def transactionsStream(address: WalletAddress): ZStream[Any, TransactionsGetError, Transaction]

  def transactionsStream(address: WalletAddress, startFrom: Instant): ZStream[Any, TransactionsGetError, Transaction]

  def fetchTransaction(txHash: String): Task[Transaction]
}

object EthBlockchainRepo {
  def fetchTransactions(address: WalletAddress): ZIO[Has[EthBlockchainRepo], Throwable, List[Transaction]] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransactions(address))

  def fetchTransaction(txHash: String): ZIO[Has[EthBlockchainRepo], Throwable, Transaction] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransaction(txHash))
}
