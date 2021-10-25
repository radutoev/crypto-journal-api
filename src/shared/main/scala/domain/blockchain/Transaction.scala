package io.softwarechain.cryptojournal
package domain.blockchain

import domain.model.{
  Buy,
  Claim,
  Contribute,
  Currency,
  CurrencyPredicate,
  Fee,
  FungibleData,
  Sell,
  TransactionType,
  TransferIn,
  TransferOut,
  Unknown
}

import eu.timepit.refined

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

  /**
   * Rules for interpreting the transaction type.
   *
   * 1. If no logEvents are present => TransferIn.
   * 2. If the first event is a Swap => Buy
   * 3. If the first event is a Withdrawal => Sell
   * 4. If there are logEvents, but no decoded (is null), and if the sender_address of the first event equals to_address => Contribute
   * 5. If the name of the first event is Claimed => Claimed.
   */
  lazy val transactionType: TransactionType = logEvents.headOption.fold[TransactionType](TransferIn)(event =>
    if (event.decoded.isDefined) {
      event.decoded.get.name match {
        case "Swap"       => Buy
        case "Withdrawal" => Sell
        case "Claimed"    => Claim
        case _            => Unknown
      }
    } else {
      if (event.senderAddress == toAddress) {
        Contribute
      } else {
        Unknown
      }
    }
  )

  lazy val instant: Instant = Instant.parse(blockSignedAt)

  lazy val hasTransactionEvents: Boolean = logEvents.nonEmpty

  //check against fromAddress.
  lazy val coin: Option[String] = transactionType match {
    case Buy =>
      logEvents.reverse
        .filter(event => event.decoded.isDefined && event.decoded.get.name == "Transfer")
        .find(event => event.decoded.get.params.exists(param => param.name == "to" && param.value == fromAddress))
        .flatMap(_.senderContractSymbol)
    case Sell =>
      logEvents.reverse.headOption.flatMap(_.senderContractSymbol)
    case _ => None
  }

  //Atm we work only with WBNB so we assume it as the coin used in buy/sell operations.
  lazy val fee: Fee =
    FungibleData(gasSpent * gasPrice * Math.pow(10, -18), Currency.unsafeFrom("WBNB"))

  lazy val value: Either[String, FungibleData] = transactionType match {
    case Unknown => Left("Unknown transaction type")
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
    case Sell =>
      (for {
        decoded  <- logEvents.head.decoded
        wadValue <- decoded.params.find(_.name == "wad").map(_.value).map(BigDecimal(_))
        decimals <- logEvents.head.senderContractDecimals
        amount   = wadValue * Math.pow(10, -decimals)
      } yield FungibleData(amount, Currency.unsafeFrom("WBNB")))
        .toRight("Unable to determine value of transaction")
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
          } yield FungibleData(amount * Math.pow(10, -decimals), Currency.unsafeFrom(symbol)) //note that we could have any coin symbol here.
        }
        .toRight("Unable to extract Claim transaction")
  }
}
