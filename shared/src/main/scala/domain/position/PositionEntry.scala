package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{ LogEvent, Transaction }
import domain.model.{ Currency, Fee, FungibleData, TransactionHash, WBNB, WalletAddress, WalletAddressPredicate }
import util.{ ListEitherOps, ListOptionOps }

import eu.timepit.refined.refineV

import java.time.Instant
import scala.util.Try

sealed trait PositionEntry {
  val hash: TransactionHash
  val fee: Fee
  val timestamp: Instant
}

object PositionEntry {
  //TODO By having this in PositionEntry I am making it aware of blockchain transactions. I need to move it somewhere else.
  def fromTransaction(transaction: Transaction, walletAddress: WalletAddress): Either[String, List[PositionEntry]] =
    if (transaction.isAirDrop()) {
      txToAirDrop(transaction, walletAddress).map(List(_))
    } else if (transaction.isApproval()) {
      txToApproval(transaction).map(List(_))
    } else if (transaction.isBuy()) {
      txToBuy(transaction, walletAddress)
    } else if (transaction.isClaim()) {
      txToClaim(transaction, walletAddress).map(List(_))
    } else if (transaction.isContribute()) {
      txToContribute(transaction).map(List(_))
    } else if (transaction.isSale()) {
      txToSell(transaction, walletAddress)
    } else if (transaction.isTransferIn()) {
      txToTransferIn(transaction).map(List(_))
    } else if (transaction.isTransferOut(walletAddress)) {
      txToTransferOut(transaction, walletAddress).map(List(_))
    } else {
      Left("Unable to interpret transaction")
    }

  private def txToAirDrop(transaction: Transaction, walletAddress: WalletAddress): Either[String, AirDrop] = {
    lazy val amountOfCoins = transaction
      .transferEventsToWallet(walletAddress)
      .map(ev => ev.paramValue("value").map(BigDecimal(_)))
      .values
      .sum

    for {
      first       <- transaction.firstTransferEvent()
      currency    <- first.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
      decimals    <- first.senderContractDecimals.toRight("Did not find contract decimals")
      finalAmount = amountOfCoins * Math.pow(10, -decimals)
      rawAddress  <- first.paramValue("from").toRight("Did not find sender address")
      sender      <- refineV[WalletAddressPredicate](rawAddress)
    } yield AirDrop(
      sender,
      transaction.computedFee(),
      FungibleData(finalAmount, currency),
      transaction.hash,
      transaction.instant
    )
  }

  private def txToApproval(transaction: Transaction): Either[String, Approval] =
    refineV[WalletAddressPredicate](transaction.toAddress)
      .map(toContract => Approval(transaction.computedFee(), toContract, transaction.hash, transaction.instant))

  //we consider the last transfer as th BUY, the rest are TransferIns.
  private def txToBuy(transaction: Transaction, address: WalletAddress): Either[String, List[PositionEntry]] = {
    val transfersToAddress = transaction.logEvents.filter(_.isTransferToAddress(address))

    if (transfersToAddress.nonEmpty) {
      for {
        depositEvent <- transaction.depositEvent()
        depositDst <- depositEvent
                       .paramValue("dst")
                       .toRight("Did not find destination")
                       .flatMap(refineV[WalletAddressPredicate](_))
        transfersFromDeposit = transaction.logEvents.filter(_.isTransferFromAddress(depositDst))
        depositDestinations = transfersFromDeposit
          .map(_.paramValue("to"))
          .values
          .map(refineV[WalletAddressPredicate](_))
          .rights
        buyCandidates = transaction.logEvents.filter(ev =>
          ev.isTransferToAddress(address) && depositDestinations.exists(ev.isTransferFromAddress(_))
        )
        buy <- if (buyCandidates.size != 1) Left("Unable to identify buy event") else Right(buyCandidates.head)
        transferIns = transaction.logEvents.filter(ev =>
          ev.isTransferToAddress(address) && ev.paramValue("from") != buy.paramValue("from")
        )
        fee      = transaction.computedFee()
        decimals <- depositEvent.senderContractDecimals.toRight("Did not find contract decimals")
        amountSpent <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -decimals)).toEither.left.map(_ =>
                        "Cannot determine amount spent"
                      )
        currency                <- depositEvent.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
        spent                   = FungibleData(amountSpent, currency)
        (coinAddress, received) <- dataFromTransferInEvent(buy)
      } yield Buy(fee, spent, received, coinAddress, transaction.hash, transaction.instant) :: transferIns
        .map(dataFromTransferInEvent)
        .rights
        .map {
          case (address, data) =>
            TransferIn(data, address, FungibleData.zero(WBNB), transaction.hash, transaction.instant)
        }
    } else {
      Left("Unable to extract Buy from transaction")
    }
  }

  private def txToClaim(transaction: Transaction, address: WalletAddress): Either[String, Claim] = {
    lazy val amountOfCoins = transaction
      .transferEventsToWallet(address)
      .map(ev => ev.paramValue("value").map(BigDecimal(_)))
      .values
      .sum

    for {
      first       <- transaction.firstTransferEvent()
      currency    <- first.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
      decimals    <- first.senderContractDecimals.toRight("Did not find contract decimals")
      finalAmount = amountOfCoins * Math.pow(10, -decimals)
      rawAddress  <- first.paramValue("from").toRight("Did not find sender address")
      sender      <- refineV[WalletAddressPredicate](rawAddress)
    } yield Claim(
      transaction.computedFee(),
      FungibleData(finalAmount, currency),
      sender,
      transaction.hash,
      transaction.instant
    )
  }

  private def txToContribute(transaction: Transaction): Either[String, Contribute] =
    for {
      txValue <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -18)).toEither.left.map(_ =>
                  "Cannot determine amount"
                )
      toAddress <- refineV[WalletAddressPredicate](transaction.toAddress)
    } yield Contribute(
      FungibleData(txValue, WBNB),
      toAddress,
      transaction.computedFee(),
      transaction.hash,
      transaction.instant
    )

  //Looks for a withdrawal event, if not found then looks for a swap.
  private def txToSell(transaction: Transaction, walletAddress: WalletAddress): Either[String, List[PositionEntry]] = {
    val transfersToWallet   = transaction.transferEventsToWallet(walletAddress)
    val transfersFromWallet = transaction.transferEventsFromWallet(walletAddress)

    @inline
    def asTransferIn(ev: LogEvent): Either[String, TransferIn] =
      for {
        decimals <- ev.senderContractDecimals.toRight("Did not find decimals")
        currency <- ev.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
        amount   <- ev.paramValue("value").map(BigDecimal(_)).toRight("Did not find amount")
        data     = FungibleData(amount * Math.pow(10, -decimals), currency)
        from     <- ev.paramValue("from").toRight("Did not find sender").flatMap(refineV[WalletAddressPredicate](_))
      } yield TransferIn(data, from, FungibleData.zero(WBNB), transaction.hash, transaction.instant)

    val (receivedCandidate, receivedAmount, transferIns) = {
      val maybeWithdrawal = transaction.logEvents.find(_.isWithdrawal())
      maybeWithdrawal.fold(
        (
          transfersToWallet.headOption,
          transfersToWallet.headOption.flatMap(_.paramValue("value")),
          transfersToWallet.tail
        )
      )(withdrawal => (Some(withdrawal), withdrawal.paramValue("wad"), transfersToWallet))
    }

    val sellEither = for {
      candidate <- receivedCandidate.toRight("No Withdrawal event")
      amount    <- receivedAmount.map(BigDecimal(_)).toRight("Did not find amount")
      decimals  <- candidate.senderContractDecimals.toRight("Did not find decimals")
      currency  <- candidate.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
      received  = FungibleData(amount * Math.pow(10, -decimals), currency)
      sold = transfersFromWallet.map { ev =>
        for {
          decimals <- ev.senderContractDecimals.toRight("Did not find decimals")
          currency <- ev.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
          amount   <- ev.paramValue("value").map(BigDecimal(_)).toRight("Did not find amount")
        } yield FungibleData(amount * Math.pow(10, -decimals), currency)
      }.rights
      //union fails if there are no transfers from our address.
      //union also has the assumption of unique currency
      soldUnion = sold.foldLeft(FungibleData.zero(sold.head.currency))((acc, el) => acc.add(el.amount))
    } yield Sell(soldUnion, received, transaction.computedFee(), transaction.hash, transaction.instant)

    sellEither.map(sell => transferIns.map(asTransferIn).rights :+ sell)
  }

  private def txToTransferIn(transaction: Transaction): Either[String, TransferIn] =
    for {
      txValue <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -18)).toEither.left.map(_ =>
                  "Cannot determine amount"
                )
      receivedFrom <- refineV[WalletAddressPredicate](transaction.fromAddress)
    } yield TransferIn(
      FungibleData(txValue, WBNB),
      receivedFrom,
      transaction.computedFee(),
      transaction.hash,
      transaction.instant
    )

  private def txToTransferOut(transaction: Transaction, address: WalletAddress): Either[String, TransferOut] = {
    val transferOuts = transaction.logEvents.filter(_.isTransferFromAddress(address))
    for {
      decimals <- transferOuts.headOption.flatMap(_.senderContractDecimals).toRight("Did not find contract decimals")
      currency <- transferOuts.headOption
                   .flatMap(_.senderContractSymbol)
                   .toRight("Did not find currency")
                   .flatMap(Currency(_))
      toAddress <- transferOuts.headOption
                    .flatMap(_.paramValue("to"))
                    .toRight("Did not find destination address")
                    .flatMap(refineV[WalletAddressPredicate](_))
      amount = transferOuts.map(_.paramValue("value").map(BigDecimal(_))).values.sum * Math.pow(10, -decimals)
    } yield TransferOut(
      FungibleData(amount, currency),
      toAddress,
      transaction.computedFee(),
      transaction.hash,
      transaction.instant
    )
  }

  private def dataFromTransferInEvent(event: LogEvent): Either[String, (WalletAddress, FungibleData)] =
    for {
      senderDecimals <- event.senderContractDecimals.toRight("Did not find contract decimals")
      rawCurrency    <- event.senderContractSymbol.toRight("Did not find currency")
      currency       <- Currency(rawCurrency)
      rawAmount      <- event.paramValue("value").toRight("Cannot determine amount")
      amount <- Try(BigDecimal(rawAmount) * Math.pow(10, -senderDecimals)).toEither.left.map(_ =>
                 "Cannot determine amount"
               )
      senderAddress <- refineV[WalletAddressPredicate](event.senderAddress)
    } yield (senderAddress, FungibleData(amount, currency))

  implicit class TransactionOps(transaction: Transaction) {
    def depositEvent(): Either[String, LogEvent] =
      transaction.logEvents
        .find(ev => isDepositEvent(ev) && ev.paramValue("dst").contains(transaction.toAddress))
        .toRight("Unable to interpret Deposit event")

    /**
     * Finds the last transfer event that was sent to the user's wallet.
     * Log events are in inverse chronological order, so no need to reverse
     * @return
     */
    def lastTransferEventToWallet(): Either[String, LogEvent] =
      transaction.logEvents
        .find(ev => ev.isTransferEvent() && ev.paramValue("to").contains(transaction.fromAddress))
        .toRight("Unable to interpret Transfer event")

    def firstTransferEvent(): Either[String, LogEvent] =
      transaction.logEvents
        .findLast(_.isTransferEvent())
        .toRight("Unable to interpret Transfer event")

    def transferEventsToWallet(address: WalletAddress): List[LogEvent] =
      transaction.logEvents
        .filter(ev => ev.isTransferEvent() && ev.paramValue("to").contains(address.value))

    def transferEventsFromWallet(address: WalletAddress): List[LogEvent] =
      transaction.logEvents
        .filter(ev => ev.isTransferEvent() && ev.paramValue("from").contains(address.value))

    private def isDepositEvent(event: LogEvent): Boolean =
      event.decoded.exists(_.name == "Deposit")

    private def isApprovalEvent(logEvent: LogEvent): Boolean =
      logEvent.decoded.exists(_.name == "Approval")

    def isAirDrop(): Boolean =
      if (transaction.logEvents.nonEmpty) {
        val eventsInChronologicalOrder = transaction.logEvents.reverse
        (for {
          firstTransferValue <- eventsInChronologicalOrder.head.paramValue("value").map(BigDecimal(_))
          valueForAllTransfers = eventsInChronologicalOrder.tail
            .filter(_.isTransferEvent())
            .map(_.paramValue("value").map(BigDecimal(_)))
            .collect {
              case Some(value) => value
            }
            .sum
        } yield transaction.rawValue.toDouble == 0d && valueForAllTransfers == firstTransferValue).getOrElse(false)
      } else {
        false
      }

    def isApproval(): Boolean =
      transaction.logEvents.size == 1 && transaction.logEvents.exists(ev =>
        isApprovalEvent(ev) && ev.paramValue("owner").contains(transaction.fromAddress)
      )

    def isBuy(): Boolean =
      transaction.rawValue.toDouble != 0d && transaction.logEvents.exists(ev =>
        ev.decoded.exists(decoded =>
          decoded.name == "Transfer" && decoded.params.exists(param =>
            param.name == "to" && param.`type` == "address" && param.value == transaction.fromAddress
          )
        )
      )

    def isClaim(): Boolean =
      transaction.logEvents.headOption.exists(ev => ev.decoded.exists(d => d.name == "Claimed"))

    def isContribute(): Boolean =
      transaction.rawValue.toDouble != 0d && transaction.logEvents.headOption.exists(ev =>
        ev.decoded.isEmpty && ev.senderAddress == transaction.toAddress
      )

    def isSale(): Boolean =
      transaction.logEvents.exists(ev =>
        ev.decoded.exists(decoded =>
          decoded.name == "Swap" &&
            decoded.params.exists(param =>
              param.name == "sender" && param.`type` == "address" && param.value == transaction.toAddress
            )
        )
      )

    def isTransferIn(): Boolean =
      transaction.logEvents.isEmpty

    def isTransferOut(address: WalletAddress): Boolean =
      transaction.rawValue.toDouble == 0d &&
        transaction.logEvents.exists(_.isTransferFromAddress(address))

    def computedFee(): Fee = FungibleData(transaction.gasSpent * transaction.gasPrice * Math.pow(10, -18), WBNB)
  }

  implicit class LogEventOps(logEvent: LogEvent) {
    def isApproval(): Boolean = logEvent.decoded.exists(_.name == "Approval")

    def isTransferEvent(): Boolean = logEvent.decoded.exists(_.name == "Transfer")

    def isWithdrawal(): Boolean = logEvent.decoded.exists(_.name == "Withdrawal")

    def isSwap(): Boolean = logEvent.decoded.exists(_.name == "Swap")

    def paramValue(paramName: String): Option[String] =
      logEvent.decoded.flatMap(_.params.find(_.name == paramName).map(_.value))

    def isTransferFromAddress(address: WalletAddress) =
      isTransferEvent() && paramValue("from").contains(address.value)

    def isTransferToAddress(address: WalletAddress) =
      isTransferEvent() && paramValue("to").contains(address.value)

    def isSwapToAddress(address: WalletAddress) =
      isSwap() && paramValue("to").contains(address.value)
  }
}

final case class AirDrop(
  receivedFrom: WalletAddress,
  fee: Fee,
  received: FungibleData,
  hash: TransactionHash,
  timestamp: Instant
) extends PositionEntry

final case class Approval(fee: Fee, forContract: WalletAddress, hash: TransactionHash, timestamp: Instant)
    extends PositionEntry

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

final case class Contribute(spent: FungibleData, to: WalletAddress, fee: Fee, hash: TransactionHash, timestamp: Instant)
    extends PositionEntry

final case class Sell(sold: FungibleData, received: FungibleData, fee: Fee, hash: TransactionHash, timestamp: Instant)
    extends PositionEntry

final case class TransferIn(
  value: FungibleData,
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
