package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{LogEvent, Transaction}
import domain.model.{Currency, Fee, FungibleData, TransactionHash, WBNB, WalletAddress, WalletAddressPredicate}

import eu.timepit.refined.refineV

import java.time.Instant
import scala.util.Try

sealed trait PositionEntry {
  val hash: TransactionHash
  val fee: Fee
  val timestamp: Instant
}

object PositionEntry {
  def fromTransaction(transaction: Transaction): Either[String, PositionEntry] =
    txToBuy(transaction)

  private def txToBuy(transaction: Transaction): Either[String, Buy] =
    for {
      depositEvent <- transaction.depositEvent()
      fee          = txFee(transaction)
      decimals     <- depositEvent.senderContractDecimals.toRight("Did not find contract decimals")
      amountSpent <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -decimals)).toEither.left.map(_ =>
                      "Cannot determine amount spent"
                    )
      rawCurrency             <- depositEvent.senderContractSymbol.toRight("Did not find currency")
      currency                <- Currency(rawCurrency)
      spent                   = FungibleData(amountSpent, currency)
      transferEvent           <- transaction.lastTransferEventToWallet()
      (coinAddress, received) <- dataFromTransferEvent(transferEvent)
    } yield Buy(fee, spent, received, coinAddress, transaction.hash, transaction.instant)

  private def dataFromTransferEvent(event: LogEvent): Either[String, (WalletAddress, FungibleData)] =
    for {
      senderDecimals <- event.senderContractDecimals.toRight("Did not find contract decimals")
      rawCurrency    <- event.senderContractSymbol.toRight("Did not find currency")
      currency       <- Currency(rawCurrency)
      rawAmount      <- readParamValue(event, "value").toRight("Cannot determine amount")
      amount <- Try(BigDecimal(rawAmount) * Math.pow(10, -senderDecimals)).toEither.left.map(_ =>
                 "Cannot determine amount"
               )
      senderAddress <- refineV[WalletAddressPredicate](event.senderAddress)
    } yield (senderAddress, FungibleData(amount, currency))

  private def txFee(tx: Transaction): Fee =
    FungibleData(tx.gasSpent * tx.gasPrice * Math.pow(10, -18), WBNB)

  implicit class TransactionOps(transaction: Transaction) {
    def depositEvent(): Either[String, LogEvent] =
      transaction.logEvents
        .find(ev => isDeposit(ev) && readParamValue(ev, "dst").contains(transaction.toAddress))
        .toRight("Unable to interpret Deposit event")

    /**
     * Finds the last transfer event that was sent to the user's wallet.
     * Log events are in inverse chronological order, so no need to reverse
     * @return
     */
    def lastTransferEventToWallet(): Either[String, LogEvent] =
      transaction.logEvents
        .find(ev => isTransfer(ev) && readParamValue(ev, "to").contains(transaction.fromAddress))
        .toRight("Unable to interpret Transfer event")

    private def isDeposit(event: LogEvent): Boolean =
      event.decoded.exists(_.name == "Deposit")

    private def isTransfer(event: LogEvent): Boolean = event.decoded.exists(_.name == "Transfer")
  }

  private def readParamValue(logEvent: LogEvent, paramName: String): Option[String] =
    logEvent.decoded.flatMap(_.params.find(_.name == paramName).map(_.value))
}

final case class AirDrop(
  receivedFrom: WalletAddress,
  fee: Fee,
  received: FungibleData,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry

final case class Approval(fee: Fee, hash: TransactionHash, timestamp: Instant) extends PositionEntry

final case class Buy(
  fee: Fee,
  spent: FungibleData,
  received: FungibleData,
  coinAddress: WalletAddress,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry

final case class Claim(
  fee: Fee,
  received: FungibleData,
  receivedFrom: WalletAddress,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry

final case class Contribute(spent: FungibleData, fee: Fee, hash: TransactionHash, timestamp: Instant)
    extends PositionEntry

final case class TransferIn(
  amount: FungibleData,
  receivedFrom: WalletAddress,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry

//TODO transfer out represents me sending out coins to another address, but what happens when I want to exchange coins for FIAT??
final case class TransferOut(
  amount: FungibleData,
  to: WalletAddress,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry
