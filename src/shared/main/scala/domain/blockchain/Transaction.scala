package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{Approval, Buy, Claim, Contribute, Currency, Fee, FungibleData, Sell, TransactionType, TransferIn, TransferOut, Unknown}

import java.time.Instant

/**
 * @param logEvents list of events that are part of the transaction. Latest event is first item.
 */
final case class Transaction(
  blockSignedAt: String,
  hash: String,
  successful: Boolean,
  fromAddress: String,
  fromAddressLabel: Option[String],
  toAddress: String,
  toAddressLabel: Option[String],
  rawValue: Double,
  valueQuote: Option[Double],
  gasOffered: Int,
  gasSpent: Int,
  gasPrice: Double,
  gasQuote: Double,
  gasQuoteRate: Double,
  logEvents: List[LogEvent]
) {

  lazy val transactionType: TransactionType = {
    @inline
    def isApproval(): Boolean =
      logEvents.exists(ev => isApprovalEvent(ev) && readParamValue(ev, "owner").contains(fromAddress))

    @inline
    def isBuy(): Boolean =
      rawValue != 0d && logEvents.exists(ev =>
        ev.decoded.exists(decoded =>
          decoded.name == "Transfer" && decoded.params.exists(param =>
            param.name == "to" && param.`type` == "address" && param.value == fromAddress
          )
        )
      )

    @inline
    def isClaim(): Boolean =
      logEvents.headOption.exists(ev => ev.decoded.exists(d => d.name == "Claimed"))

    @inline
    def isContribute(): Boolean = {
      rawValue != 0d && logEvents.headOption.exists(ev => ev.decoded.isEmpty && ev.senderAddress == toAddress)
    }

    @inline
    def isSale(): Boolean =
      logEvents.exists(ev =>
        ev.decoded.exists(decoded =>
          decoded.name == "Swap" &&
            decoded.params.exists(param =>
              param.name == "sender" && param.`type` == "address" && param.value == toAddress
            )
        )
      )

    @inline
    def isTransferIn(): Boolean =
      logEvents.isEmpty


    if(isApproval()) {
      Approval
    } else {
      if (isBuy()) {
        Buy
      } else {
        if (isClaim()) {
          Claim
        } else {
          if (isContribute()) {
            Contribute
          } else {
            if (isSale()) {
              Sell
            } else {
              if (isTransferIn()) {
                TransferIn
              } else {
                Unknown
              }
            }
          }
        }
      }
    }
  }

  lazy val instant: Instant = Instant.parse(blockSignedAt)

  lazy val hasTransactionEvents: Boolean = logEvents.nonEmpty

  //TODO Should I make it return Currency?
  lazy val coin: Option[String] = transactionType match {
    case Approval =>
      logEvents.find(ev => isApprovalEvent(ev) && readParamValue(ev, "owner").contains(fromAddress))
        .flatMap(_.senderContractSymbol)
    case Buy =>
      logEvents
        .find(ev => isTransferEvent(ev) && readParamValue(ev, "to").contains(fromAddress))
        .flatMap(_.senderContractSymbol)
    case Claim =>
      logEvents.filter(event => event.decoded.isDefined && event.decoded.get.name == "Transfer")
        .flatMap(_.senderContractSymbol)
        .headOption
    case Contribute =>
      Some("WBNB") //default to WBNB
    case Sell =>
      logEvents
        .find(ev => isTransferEvent(ev) && readParamValue(ev, "from").contains(fromAddress))
        .flatMap(_.senderContractSymbol)
    case TransferIn =>
      Some("WBNB") //default to WBNB
    case _ => None
  }

  //Atm we work only with WBNB so we assume it as the coin used in buy/sell operations.
  lazy val fee: Fee =
    FungibleData(gasSpent * gasPrice * Math.pow(10, -18), Currency.unsafeFrom("WBNB"))

  lazy val value: Either[String, FungibleData] = transactionType match {
    case Unknown => Left("Unknown transaction type")
    case Approval =>
      logEvents
        .find(ev => isTransferEvent(ev) && readParamValue(ev, "to").contains(fromAddress))
        .flatMap(_.senderContractSymbol)
        .map(symbol => FungibleData(BigDecimal(0), Currency.unsafeFrom(symbol)))
        .toRight("Unable to interpret Approval event")
    case Buy =>
      (for {
        decoded <- logEvents.last.decoded
        wadValue <- decoded.params
                     .find(_.name == "wad")
                     .map(_.value)
                     .map(BigDecimal(_))
        decimals <- logEvents.last.senderContractDecimals
        amount   = wadValue * Math.pow(10, -decimals)
      } yield FungibleData(amount, Currency.unsafeFrom("WBNB")))
        .toRight("Unable to determine value of transaction")
    case Sell => getValueOfLatestSwapEvent()
    case TransferIn =>
      Right(
        FungibleData(
          //I have no log events here, so assuming wei as ETH subunit with a default decimal count of -18 for the contract
          amount = BigDecimal(rawValue) * Math.pow(10, -18),
          currency = Currency.unsafeFrom("WBNB")
        )
      )
    case TransferOut =>
      Right(
        FungibleData(
          //I have no log events here, so assuming wei as ETH subunit with a default decimal count of -18 for the contract
          amount = BigDecimal(rawValue) * Math.pow(10, -18),
          currency = Currency.unsafeFrom("WBNB")
        )
      )
    case Contribute =>
      Right(
        FungibleData(
          amount = BigDecimal(rawValue) * Math.pow(10, -18),
          currency = Currency.unsafeFrom("WBNB")
        )
      )
    case Claim =>
      logEvents
        .find(ev => ev.decoded.isDefined && ev.decoded.get.name == "Transfer")
        .flatMap { logEvent =>
          for {
            amount   <- logEvent.decoded.get.params.last.value.toLongOption.map(BigDecimal(_))
            decimals <- logEvent.senderContractDecimals
            symbol   <- logEvent.senderContractSymbol
          } yield FungibleData(
            amount * Math.pow(10, -decimals),
            Currency.unsafeFrom(symbol)
          ) //note that we could have any coin symbol here.
        }
        .toRight("Unable to extract Claim transaction")
  }

  private def getValueOfLatestSwapEvent(): Either[String, FungibleData] =
    (for {
      swapEvent <- logEvents.find(_.decoded.exists(_.name == "Swap"))
      decimals  <- swapEvent.senderContractDecimals
      amount    <- extractAmountFromSwapEvent(swapEvent)
    } yield FungibleData(amount * Math.pow(10, -decimals), Currency.unsafeFrom("WBNB")))
      .toRight("Unable to determine value of transaction")

  private def extractAmountFromSwapEvent(event: LogEvent): Option[BigDecimal] =
    event.decoded.flatMap { decoded =>
      if (toAddressMatchesTransactionFromAddress(decoded.params)) {
        decoded.params.find(param => param.name == "amount0In").map(_.value).map(BigDecimal(_))
      } else {
        if (toAddressMatchesTransactionToAddress(decoded.params)) {
          decoded.params.find(param => param.name == "amount1Out").map(_.value).map(BigDecimal(_))
        } else {
          None
        }
      }
    }

  private def toAddressMatchesTransactionFromAddress(params: List[Param]): Boolean =
    params.exists(param => param.name == "to" && param.`type` == "address" && param.value == fromAddress)

  private def toAddressMatchesTransactionToAddress(params: List[Param]): Boolean =
    params.exists(param => param.name == "to" && param.`type` == "address" && param.value == toAddress)

  private def isApprovalEvent(logEvent: LogEvent): Boolean = {
    logEvent.decoded.exists(_.name == "Approval")
  }

  private def isTransferEvent(logEvent: LogEvent): Boolean = {
    logEvent.decoded.exists(_.name == "Transfer")
  }

  private def readParamValue(logEvent: LogEvent, paramName: String): Option[String] = {
    logEvent.decoded.flatMap(_.params.find(_.name == paramName).map(_.value))
  }
}
