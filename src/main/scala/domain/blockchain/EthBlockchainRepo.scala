package io.softwarechain.cryptojournal
package domain.blockchain

import zio.{Has, Task, ZIO}

trait EthBlockchainRepo {
  def fetchTransactions(walletAddress: String): Task[List[Transaction]]

  def fetchTransaction(txHash: String): Task[Transaction]
}

object EthBlockchainRepo {
  def fetchTransactions(walletAddress: String): ZIO[Has[EthBlockchainRepo], Throwable, List[Transaction]] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransactions(walletAddress))

  def fetchTransaction(txHash: String): ZIO[Has[EthBlockchainRepo], Throwable, Transaction] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransaction(txHash))
}