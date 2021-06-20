package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.WalletAddress

import zio.stream.ZStream
import zio.{Has, Task, ZIO}

trait EthBlockchainRepo {
  def fetchTransactions(address: WalletAddress): Task[List[Transaction]]

  def fetchTransaction(txHash: String): Task[Transaction]
}

object EthBlockchainRepo {
  def fetchTransactions(address: WalletAddress): ZIO[Has[EthBlockchainRepo], Throwable, List[Transaction]] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransactions(address))

  def fetchTransaction(txHash: String): ZIO[Has[EthBlockchainRepo], Throwable, Transaction] =
    ZIO.serviceWith[EthBlockchainRepo](_.fetchTransaction(txHash))
}
