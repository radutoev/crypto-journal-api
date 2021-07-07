package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.{
  Transaction => DomainTransaction,
  LogEvent => DomainLogEvent,
  Decoded => DomainDecoded,
  Param => DomainParam
}

import zio.json.{ jsonField, DeriveJsonCodec, DeriveJsonDecoder, JsonCodec, JsonDecoder }

object dto {
  final case class TransactionQueryResponse(
    data: TransactionResponse,
    error: Boolean,
    @jsonField("error_code") errorCode: Option[Int]
  )

  object TransactionQueryResponse {
    implicit val encoder: JsonDecoder[TransactionQueryResponse] = DeriveJsonDecoder.gen[TransactionQueryResponse]
  }

  final case class TransactionResponse(quote_currency: Option[String], items: List[Transaction], pagination: Pagination)

  object TransactionResponse {
    implicit val encoder: JsonDecoder[TransactionResponse] = DeriveJsonDecoder.gen[TransactionResponse]
  }

  final case class Transaction(
    @jsonField("block_signed_at") blockSignedAt: String,
    @jsonField("tx_hash") hash: String,
    successful: Boolean,
    @jsonField("from_address") fromAddress: String,
    @jsonField("from_address_label") fromAddressLabel: Option[String],
    @jsonField("to_address") toAddress: String,
    @jsonField("to_address_label") toAddressLabel: Option[String],
    @jsonField("value") rawValue: Double,
    @jsonField("value_quote") valueQuote: Option[Double],
    @jsonField("gas_offered") gasOffered: Int,
    @jsonField("gas_spent") gasSpent: Int,
    @jsonField("gas_price") gasPrice: Double,
    @jsonField("gas_quote") gasQuote: Double,
    @jsonField("gas_quote_rate") gasQuoteRate: Double,
    @jsonField("log_events") logEvents: List[LogEvent]
  )

  object Transaction {
    implicit val encoder: JsonCodec[Transaction] = DeriveJsonCodec.gen[Transaction]

    implicit class TransactionOps(transaction: Transaction) {
      def toDomain(): DomainTransaction =
        //I get null on decoded, figure out why that happens
        DomainTransaction(
          blockSignedAt = transaction.blockSignedAt,
          hash = transaction.hash,
          successful = transaction.successful,
          fromAddress = transaction.fromAddress,
          fromAddressLabel = transaction.fromAddressLabel,
          toAddress = transaction.toAddress,
          toAddressLabel = transaction.toAddressLabel,
          rawValue = transaction.rawValue,
          valueQuote = transaction.valueQuote,
          gasOffered = transaction.gasOffered,
          gasSpent = transaction.gasSpent,
          gasPrice = transaction.gasPrice,
          gasQuote = transaction.gasQuote,
          gasQuoteRate = transaction.gasQuoteRate,
          logEvents = transaction.logEvents.filter(_.decoded.isDefined).map(_.toDomain())
        )
    }
  }

  final case class LogEvent(
    @jsonField("sender_contract_decimals") senderContractDecimals: Option[Int],
    @jsonField("sender_name") senderName: Option[String],
    @jsonField("sender_contract_ticker_symbol") senderContractSymbol: Option[String],
    @jsonField("sender_address") senderAddress: String,
    @jsonField("sender_address_label") senderAddressLabel: Option[String],
    decoded: Option[Decoded]
  )

  object LogEvent {
    implicit val encoder: JsonCodec[LogEvent] = DeriveJsonCodec.gen[LogEvent]

    implicit class LogEventOps(logEvent: LogEvent) {
      def toDomain() =
        DomainLogEvent(
          senderContractDecimals = logEvent.senderContractDecimals,
          senderName = logEvent.senderName,
          senderContractSymbol = logEvent.senderContractSymbol,
          senderAddress = logEvent.senderAddress,
          senderAddressLabel = logEvent.senderAddressLabel,
          decoded = logEvent.decoded.get.toDomain() //force get as i filter None values
        )
    }
  }

  final case class Decoded(name: String, signature: String, params: List[Param])

  object Decoded {
    implicit val encoder: JsonCodec[Decoded] = DeriveJsonCodec.gen[Decoded]

    implicit class DecodedOps(decoded: Decoded) {
      def toDomain() =
        DomainDecoded(name = decoded.name, signature = decoded.signature, params = decoded.params.map(_.toDomain()))
    }
  }

  final case class Param(name: String, `type`: String, indexed: Boolean, decoded: Boolean, value: String)

  object Param {
    implicit val encoder: JsonCodec[Param] = DeriveJsonCodec.gen[Param]

    implicit class ParamOps(param: Param) {
      def toDomain() =
        DomainParam(
          name = param.name,
          `type` = param.`type`,
          indexed = param.indexed,
          decoded = param.decoded,
          value = param.value
        )
    }
  }

  final case class Pagination(
    @jsonField("has_more") hasMore: Boolean,
    @jsonField("page_number") pageNumber: Int,
    @jsonField("page_size") pageSize: Int
  )

  object Pagination {
    implicit val encoder: JsonDecoder[Pagination] = DeriveJsonDecoder.gen[Pagination]
  }
}
