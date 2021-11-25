package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.{ LogEvent, Transaction }
import domain.model._
import domain.position.PositionEntry.PositionEntryId
import util.{ ListEitherOps, ListOptionOps }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.domain.position.model.{ CoinName, CoinNamePredicate }

import java.time.Instant
import scala.util.Try

sealed trait PositionEntry {
  val hash: TransactionHash
  val fee: Fee
  val timestamp: Instant
  val id: Option[PositionEntryId] = None
}

object PositionEntry {
  type PositionEntryIdPredicate = NonEmpty
  type PositionEntryId          = String Refined PositionEntryIdPredicate

  //TODO By having this in PositionEntry I am making it aware of blockchain transactions. I need to move it somewhere else.
  def fromTransaction(transaction: Transaction, walletAddress: WalletAddress): Either[String, List[PositionEntry]] = {
    lazy val airDropData     = transaction.airDropData(walletAddress)
    lazy val approvalData    = transaction.approvalData(walletAddress)
    lazy val buyData         = transaction.buyData(walletAddress)
    lazy val claimData       = transaction.claimData(walletAddress)
    lazy val contributeData  = transaction.contributeData(walletAddress)
    lazy val sellData        = transaction.sellData(walletAddress)
    lazy val transferInData  = transaction.transferInData(walletAddress)
    lazy val transferOutData = transaction.transferOutData(walletAddress)

    if (airDropData.isDefined) {
      airDropData.get
    } else if (approvalData.isDefined) {
      approvalData.get
    } else if (buyData.isDefined) {
      buyData.get
    } else if (claimData.isDefined) {
      claimData.get
    } else if (contributeData.isDefined) {
      contributeData.get
    } else if (sellData.isDefined) {
      sellData.get
    } else if (transferInData.isDefined) {
      transferInData.get
    } else if (transferOutData.isDefined) {
      transferOutData.get
    } else {
      Left(s"Unable to interpret transaction ${transaction.hash.value}")
    }
  }

  private def dataFromTransferInEvent(
    event: LogEvent
  ): Either[String, (CoinAddress, WalletAddress, CoinName, FungibleData)] =
    for {
      senderDecimals <- event.senderContractDecimals.toRight("Did not find contract decimals")
      rawCurrency    <- event.senderContractSymbol.toRight("Did not find currency")
      currency       <- Currency(rawCurrency)
      rawAmount      <- event.paramValue("value").toRight("Cannot determine amount")
      amount <- Try(BigDecimal(rawAmount) * Math.pow(10, -senderDecimals)).toEither.left.map(_ =>
                 "Cannot determine amount"
               )
      senderAddress <- refineV[CoinAddressPredicate](event.senderAddress)
      rawFrom       <- event.paramValue("from").toRight("Cannot determine from value")
      from          <- refineV[WalletAddressPredicate](rawFrom)
      name          <- event.senderName.toRight("Did not find coin name").flatMap(refineV[CoinNamePredicate](_))
    } yield (senderAddress, from, name, FungibleData(amount, currency))

  implicit class TransactionOps(transaction: Transaction) {
    def depositEvent(): Either[String, LogEvent] =
      transaction.logEvents
        .find(ev => isDepositEvent(ev) && ev.paramValue("dst").contains(transaction.toAddress))
        .toRight("Unable to interpret Deposit event")

    def firstTransferEvent(): Either[String, LogEvent] =
      transaction.logEvents
        .findLast(_.isTransferEvent)
        .toRight("Unable to interpret Transfer event")

    def transferEventsToWallet(address: WalletAddress): List[LogEvent] =
      transaction.logEvents
        .filter(ev => ev.isTransferEvent && ev.paramValue("to").contains(address.value))

    def transferEventsFromWallet(address: WalletAddress): List[LogEvent] =
      transaction.logEvents
        .filter(ev => ev.isTransferEvent && ev.paramValue("from").contains(address.value))

    private def isDepositEvent(event: LogEvent): Boolean =
      event.decoded.exists(_.name == "Deposit")

    private def isApprovalEvent(logEvent: LogEvent): Boolean =
      logEvent.decoded.exists(_.name == "Approval")

    def airDropData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val isAirDrop = if (transaction.logEvents.nonEmpty) {
        val eventsInChronologicalOrder = transaction.logEvents.reverse
        (for {
          firstTransferValue <- eventsInChronologicalOrder.head.paramValue("value").map(BigDecimal(_))
          valueForAllTransfers = eventsInChronologicalOrder.tail
            .filter(_.isTransferEvent)
            .map(_.paramValue("value").map(BigDecimal(_)))
            .collect {
              case Some(value) => value
            }
            .sum
        } yield transaction.rawValue.toDouble == 0d && valueForAllTransfers == firstTransferValue).getOrElse(false)
      } else {
        false
      }

      if (isAirDrop) {
        val transfersToAddress = transaction.transferEventsToWallet(address)
        lazy val amountOfCoins = transfersToAddress
          .map(ev => ev.paramValue("value").map(BigDecimal(_)))
          .values
          .sum

        val airDrops = for {
          first       <- transfersToAddress.headOption.toRight(s"No transfers to address $address were found")
          currency    <- first.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
          decimals    <- first.senderContractDecimals.toRight("Did not find contract decimals")
          coinAddress <- refineV[CoinAddressPredicate](first.senderAddress)
          finalAmount = amountOfCoins * Math.pow(10, -decimals)
          sender      <- refineV[WalletAddressPredicate](transaction.fromAddress)
          rawCoinName <- first.senderName.toRight("Did not find coin name")
          coinName    <- refineV[CoinNamePredicate](rawCoinName)
        } yield List(
          AirDrop(
            coinName,
            sender,
            transaction.computedFee(),
            FungibleData(finalAmount, currency),
            coinAddress,
            transaction.hash,
            transaction.instant
          )
        )

        Some(airDrops)
      } else {
        None
      }
    }

    def approvalData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val isApproval = transaction.initiatedByAddress(address) &&
        transaction.logEvents.size == 1 && transaction.logEvents.exists(ev =>
        isApprovalEvent(ev) && ev.paramValue("owner").contains(transaction.fromAddress)
      )

      if (isApproval) {
        Some {
          refineV[WalletAddressPredicate](transaction.toAddress)
            .map(toContract =>
              List(Approval(transaction.computedFee(), toContract, transaction.hash, transaction.instant))
            )
        }
      } else {
        None
      }
    }

    def buyData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val isBnbBuy = transaction.initiatedByAddress(address) &&
        transaction.rawValue.toDouble != 0d &&
        transaction.logEvents.exists(ev =>
          ev.decoded.exists(decoded =>
            decoded.name == "Transfer" && decoded.params.exists(param =>
              param.name == "to" && param.`type` == "address" && param.value == transaction.fromAddress
            )
          )
        )

      lazy val isBuyWithOtherCoin = {
        val transfers = transaction.logEvents.filter(_.isTransferEvent)
        //most recent is first.
        val isFirstTransferFromAddress = transfers.lastOption.exists(_.isTransferFromAddress(address))
        val isAllowedBuyCurrency =
          transfers.lastOption.exists(ev => ev.senderContractSymbol.exists(Set("BUSD").contains))
        val isLastTransferToAddress = transfers.headOption.exists(_.isTransferToAddress(address))
        isFirstTransferFromAddress && isLastTransferToAddress && isAllowedBuyCurrency
      }

      if (isBnbBuy) {
        val transfersToAddress = transaction.logEvents.filter(_.isTransferToAddress(address))
        Some {
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
              buy <- if (buyCandidates.size != 1) Left(s"Unable to identify buy event ${transaction.hash.value}")
                    else Right(buyCandidates.head)
              transferIns = transaction.logEvents.filter(ev =>
                ev.isTransferToAddress(address) && ev.paramValue("from") != buy.paramValue("from")
              )
              fee      = transaction.computedFee()
              decimals <- depositEvent.senderContractDecimals.toRight("Did not find contract decimals")
              amountSpent <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -decimals)).toEither.left.map(_ =>
                              "Cannot determine amount spent"
                            )
              currency                            <- depositEvent.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
              spent                               = FungibleData(amountSpent, currency)
              (coinAddress, from, name, received) <- dataFromTransferInEvent(buy)
            } yield Buy(fee, spent, received, from, name, coinAddress, transaction.hash, transaction.instant) :: transferIns
              .map(dataFromTransferInEvent)
              .rights
              .map {
                case (coinAddress, from, name, data) =>
                  TransferIn(
                    data,
                    from,
                    FungibleData.zero(WBNB),
                    transaction.hash,
                    transaction.instant,
                    Some(name),
                    Some(coinAddress)
                  )
              }
          } else {
            Left("Unable to extract Buy from transaction")
          }
        }
      } else if (isBuyWithOtherCoin) {
        val transfers = transaction.logEvents.filter(_.isTransferEvent)
        val buyOptional = for {
          (received, from, coinAddress, coinName) <- transfers.headOption.flatMap(ev =>
                                                      for {
                                                        amount <- ev.paramValue("value").map(BigDecimal(_))
                                                        from <- ev
                                                                 .paramValue("from")
                                                                 .flatMap(refineV[WalletAddressPredicate](_).toOption)
                                                        decimals <- ev.senderContractDecimals
                                                        currency <- ev.senderContractSymbol.flatMap(
                                                                     Currency(_).toOption
                                                                   )
                                                        name <- ev.senderName
                                                                 .toRight("coin name not found")
                                                                 .flatMap(refineV[CoinNamePredicate](_))
                                                                 .toOption
                                                        address <- refineV[CoinAddressPredicate](ev.senderAddress).toOption
                                                      } yield (
                                                        FungibleData(amount * Math.pow(10, -decimals), currency),
                                                        from,
                                                        address,
                                                        name
                                                      )
                                                    )
          spentOriginal <- transfers.lastOption.flatMap(ev =>
                            for {
                              amount   <- ev.paramValue("value").map(BigDecimal(_))
                              decimals <- ev.senderContractDecimals
                              currency <- ev.senderContractSymbol.flatMap(Currency(_).toOption)
                            } yield FungibleData(amount * Math.pow(10, -decimals), currency)
                          )
          spent <- transfers
                    .find(_.isTransferToAddress(from))
                    .flatMap(ev =>
                      for {
                        amount   <- ev.paramValue("value").map(BigDecimal(_))
                        decimals <- ev.senderContractDecimals
                        currency <- ev.senderContractSymbol.flatMap(Currency(_).toOption)
                      } yield FungibleData(amount * Math.pow(10, -decimals), currency)
                    )
        } yield Buy(
          computedFee(),
          spent,
          received,
          from,
          coinName,
          coinAddress,
          transaction.hash,
          transaction.instant,
          Some(spentOriginal)
        )

        //TODO I should look for other transferIns.
        Some(buyOptional.map(List(_)).toRight("Unable to decode Buy"))
      } else {
        None
      }
    }

    def claimData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val isClaim = transaction.initiatedByAddress(address) &&
        transaction.logEvents.headOption.exists(ev => ev.decoded.exists(d => d.name == "Claimed"))

      if (isClaim) {
        val transfersToWallet = transaction.transferEventsToWallet(address)
        lazy val amountOfCoins = transfersToWallet
          .map(ev => ev.paramValue("value").map(BigDecimal(_)))
          .values
          .sum

        Some {
          for {
            first       <- transfersToWallet.headOption.toRight("Cannot find claim event")
            currency    <- first.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
            decimals    <- first.senderContractDecimals.toRight("Did not find contract decimals")
            finalAmount = amountOfCoins * Math.pow(10, -decimals)
            rawAddress  <- first.paramValue("from").toRight("Did not find sender address")
            sender      <- refineV[WalletAddressPredicate](rawAddress)
            rawName     <- first.senderName.toRight("Cannot find coin name")
            name        <- refineV[CoinNamePredicate](rawName)
            coinAddress <- refineV[CoinAddressPredicate](first.senderAddress)
          } yield List(
            Claim(
              transaction.computedFee(),
              FungibleData(finalAmount, currency),
              sender,
              name,
              coinAddress,
              transaction.hash,
              transaction.instant
            )
          )
        }
      } else {
        None
      }
    }

    def contributeData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val isContribute = transaction.initiatedByAddress(address) &&
        transaction.rawValue.toDouble != 0d && transaction.logEvents.headOption.exists(ev =>
        ev.decoded.isEmpty && ev.senderAddress == transaction.toAddress
      )

      if (isContribute) {
        Some {
          for {
            txValue <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -18)).toEither.left.map(_ =>
                        "Cannot determine amount"
                      )
            toAddress <- refineV[WalletAddressPredicate](transaction.toAddress)
          } yield List(
            Contribute(
              FungibleData(txValue, WBNB),
              toAddress,
              transaction.computedFee(),
              transaction.hash,
              transaction.instant
            )
          )
        }
      } else {
        None
      }
    }

    def sellData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      val isSell = transaction.initiatedByAddress(address) &&
        transaction.logEvents.exists(ev =>
          ev.decoded.exists(decoded =>
            decoded.name == "Swap" &&
              decoded.params.exists(param =>
                param.name == "sender" && param.`type` == "address" && param.value == transaction.toAddress
              )
          )
        )

      if (isSell) {
        val transfersToWallet   = transaction.transferEventsToWallet(address)
        val transfersFromWallet = transaction.transferEventsFromWallet(address)

        @inline
        def asTransferIn(ev: LogEvent): Either[String, TransferIn] =
          for {
            decimals <- ev.senderContractDecimals.toRight("Did not find decimals")
            currency <- ev.senderContractSymbol.toRight("Did not find currency").flatMap(Currency(_))
            amount   <- ev.paramValue("value").map(BigDecimal(_)).toRight("Did not find amount")
            data     = FungibleData(amount * Math.pow(10, -decimals), currency)
            from     <- ev.paramValue("from").toRight("Did not find sender").flatMap(refineV[WalletAddressPredicate](_))
            name     <- ev.senderName.toRight("Did not find coin name").flatMap(refineV[CoinNamePredicate](_))
            address  <- refineV[CoinNamePredicate](ev.senderAddress)
          } yield TransferIn(
            data,
            from,
            FungibleData.zero(WBNB),
            transaction.hash,
            transaction.instant,
            Some(name),
            Some(address)
          )

        val (receivedCandidate, receivedAmount, transferIns) = {
          //I need to check if withdrawal is the last event in the transaction.
          //Withdrawal value matches the BNB value transferred to the wallet in some transactions, as part as internal transactions.
          //This means that they are not indexed, so I need to use Withdrawal as stand-in.
          //There are some transactions however that don't have internal transactions with the wallet of interest.
          //The observation is that for these types of transactions, the Withdrawal event is not the last one in the event log.
          val accountForInternalTxEvent = transaction.logEvents.head.isWithdrawal
          if (accountForInternalTxEvent) {
            val withdrawal = transaction.logEvents.head
            (Some(withdrawal), withdrawal.paramValue("wad"), transfersToWallet)
          } else {
            (
              transfersToWallet.headOption,
              transfersToWallet.headOption.flatMap(_.paramValue("value")),
              transfersToWallet.tail
            )
          }
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
          _ <- if (sold.isEmpty) Left(s"Invalid sold event for ${transaction.hash.value}") else Right(sold)
          //union fails if there are no transfers from our address.
          //union also has the assumption of unique currency
          soldUnion = sold.foldLeft(FungibleData.zero(sold.head.currency))((acc, el) => acc.add(el.amount))
        } yield Sell(soldUnion, received, transaction.computedFee(), transaction.hash, transaction.instant)

        Some(sellEither.map(sell => transferIns.map(asTransferIn).rights :+ sell))
      } else {
        None
      }
    }

    def transferInData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val noLogEvents        = transaction.logEvents.isEmpty
      lazy val hasDirectTransfers = transaction.logEvents.exists(_.isTransferToAddress(address))
      lazy val hasRefunds         = transaction.logEvents.exists(_.isRefund(address))
      if (noLogEvents && transaction.toAddress == address.value) {
        Some {
          for {
            txValue <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -18)).toEither.left.map(_ =>
                        "Cannot determine amount"
                      )
            receivedFrom <- refineV[WalletAddressPredicate](transaction.fromAddress)
          } yield List(
            TransferIn(
              FungibleData(txValue, WBNB),
              receivedFrom,
              transaction.computedFee(),
              transaction.hash,
              transaction.instant
            )
          )
        }
      } else if (hasDirectTransfers) {
        val transferIns = transaction.logEvents
          .filter(_.isTransferToAddress(address))
          .map { ev =>
            for {
              decimals <- ev.senderContractDecimals
              value    <- ev.paramValue("value").map(BigDecimal(_))
              currency <- ev.senderContractSymbol.flatMap(refineV[CurrencyPredicate](_).toOption)
              received = FungibleData(value * Math.pow(10, -decimals), currency)
              from     <- ev.paramValue("from").flatMap(refineV[WalletAddressPredicate](_).toOption)
              fee      = FungibleData.zero(WBNB) //I am not sure if the fee is 0 or not here.
            } yield TransferIn(received, from, fee, transaction.hash, transaction.instant)
          }
          .values
        Some(Right(transferIns))
      } else if (hasRefunds) {
        val refunds = transaction.logEvents
          .filter(_.isRefund(address))
          .map { ev =>
            for {
              amount <- ev.paramValue("value").map(BigDecimal(_) * Math.pow(10, -18)).toRight("Did not find value")
              from   <- refineV[WalletAddressPredicate](transaction.toAddress)
            } yield TransferIn(
              value = FungibleData(amount, WBNB),
              receivedFrom = from,
              computedFee(),
              transaction.hash,
              transaction.instant
            )
          }
          .rights
        Some(Right(refunds))
      } else {
        None
      }
    }

    def transferOutData(address: WalletAddress): Option[Either[String, List[PositionEntry]]] = {
      lazy val noLogEvents        = transaction.logEvents.isEmpty
      val txValue                 = transaction.rawValue.toDouble
      lazy val hasDirectTransfers = txValue == 0d && transaction.logEvents.exists(_.isTransferFromAddress(address))
      lazy val possibleTokenBuys  = txValue != 0d && transaction.logEvents.exists(_.isTokenPurchase(address))

      if (noLogEvents && transaction.fromAddress == address.value) {
        Some {
          for {
            txValue <- Try(BigDecimal(transaction.rawValue) * Math.pow(10, -18)).toEither.left.map(_ =>
                        "Cannot determine amount"
                      )
            toAddress <- refineV[WalletAddressPredicate](transaction.toAddress)
          } yield List(
            TransferOut(
              FungibleData(txValue, WBNB),
              toAddress,
              transaction.computedFee(),
              transaction.hash,
              transaction.instant
            )
          )
        }
      } else {
        val transferOuts: Option[Either[String, List[TransferOut]]] = if (hasDirectTransfers) {
          val tOuts = transaction.logEvents
            .filter(_.isTransferFromAddress(address))
            .map { ev =>
              for {
                currency <- ev.senderContractSymbol
                             .toRight("Did not find currency")
                             .flatMap(refineV[CurrencyPredicate](_))
                decimals <- ev.senderContractDecimals.toRight("Did not find decimals")
                toAddress <- ev
                              .paramValue("to")
                              .toRight("Did not find destination address")
                              .flatMap(refineV[WalletAddressPredicate](_))
                amount <- ev
                           .paramValue("value")
                           .map(BigDecimal(_) * Math.pow(10, -decimals))
                           .toRight("Did not find value")
              } yield TransferOut(
                FungibleData(amount, currency),
                toAddress,
                transaction.computedFee(),
                transaction.hash,
                transaction.instant
              )
            }
            .rights

          Some(Right(tOuts))
        } else if (possibleTokenBuys) {
          val candidates = transaction.logEvents.filter(_.isTokenPurchase(address)).map { ev =>
            refineV[WalletAddressPredicate](ev.senderAddress)
              .map(toAddress =>
                TransferOut(
                  FungibleData(txValue * Math.pow(10, -18), WBNB),
                  toAddress,
                  computedFee(),
                  transaction.hash,
                  transaction.instant
                )
              )
          }
          Some {
            Right {
              candidates.collect {
                case Right(tOut) => tOut
              }
            }
          }
        } else {
          None
        }

        transferOuts
      }

    }

    def initiatedByAddress(address: WalletAddress): Boolean = transaction.fromAddress == address.value

    def computedFee(): Fee = FungibleData(transaction.gasSpent * transaction.gasPrice * Math.pow(10, -18), WBNB)
  }

  implicit class LogEventOps(logEvent: LogEvent) {
    def isApproval: Boolean = logEvent.decoded.exists(_.name == "Approval")

    def isTransferEvent: Boolean = logEvent.decoded.exists(_.name == "Transfer")

    def isWithdrawal: Boolean = logEvent.decoded.exists(_.name == "Withdrawal")

    def paramValue(paramName: String): Option[String] =
      logEvent.decoded.flatMap(_.params.find(_.name == paramName).map(_.value))

    def isRefund(address: WalletAddress): Boolean =
      logEvent.decoded.exists(_.name == "Refunded") && logEvent.paramValue("from").contains(address.value)

    def isTransferFromAddress(address: WalletAddress): Boolean =
      isTransferEvent && paramValue("from").contains(address.value)

    def isTransferToAddress(address: WalletAddress): Boolean =
      isTransferEvent && paramValue("to").contains(address.value)

    def isTokenPurchase(address: WalletAddress): Boolean =
      logEvent.decoded.exists(_.name == "TokenPurchase") && paramValue("purchaser").contains(address.value)
  }
}

final case class AirDrop(
  name: CoinName,
  receivedFrom: WalletAddress,
  fee: Fee,
  received: FungibleData,
  coinAddress: CoinAddress,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class Approval(
  fee: Fee,
  forContract: WalletAddress,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class Buy(
  fee: Fee,
  spent: FungibleData,
  received: FungibleData,
  receivedFrom: WalletAddress,
  name: CoinName,
  coinAddress: CoinAddress,
  hash: TransactionHash,
  timestamp: Instant,
  spentOriginal: Option[FungibleData] = None,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class Claim(
  fee: Fee,
  received: FungibleData,
  receivedFrom: WalletAddress,
  name: CoinName,
  coinAddress: CoinAddress,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class Contribute(
  spent: FungibleData,
  to: WalletAddress,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class Sell(
  sold: FungibleData,
  received: FungibleData,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class TransferIn(
  value: FungibleData,
  receivedFrom: WalletAddress,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant,
  name: Option[CoinName] = None,
  coinAddress: Option[CoinAddress] = None,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry

final case class TransferOut(
  amount: FungibleData,
  to: WalletAddress,
  fee: Fee,
  hash: TransactionHash,
  timestamp: Instant,
  override val id: Option[PositionEntryId] = None
) extends PositionEntry
