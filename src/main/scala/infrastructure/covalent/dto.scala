package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.Transaction

import zio.json.{DeriveJsonDecoder, JsonDecoder, jsonField}

object dto {
  final case class TransactionQueryResponse(
    data: TransactionResponse,
    error: Boolean,
    @jsonField("error_code") errorCode: Int
  )

  object TransactionQueryResponse {
    implicit val encoder: JsonDecoder[TransactionQueryResponse] = DeriveJsonDecoder.gen[TransactionQueryResponse]
  }

  final case class TransactionResponse(quote_currency: Option[String], items: List[Transaction], pagination: Pagination)

  object TransactionResponse {
    implicit val encoder: JsonDecoder[TransactionResponse] = DeriveJsonDecoder.gen[TransactionResponse]
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
