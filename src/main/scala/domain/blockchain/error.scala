package io.softwarechain.cryptojournal
package domain.blockchain

object error {
  sealed trait BlockchainError
  final case class TransactionsGetError(message: String) extends BlockchainError
}
