package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.Transaction

import zio.json.{ DeriveJsonCodec, JsonCodec }

object dto {
  final case class TransactionQueryResponse(data: TransactionResponse)

  object TransactionQueryResponse {
    implicit val encoder: JsonCodec[TransactionQueryResponse] = DeriveJsonCodec.gen[TransactionQueryResponse]
  }

  final case class TransactionResponse(quote_currency: Option[String], items: List[Transaction])

  object TransactionResponse {
    implicit val encoder: JsonCodec[TransactionResponse] = DeriveJsonCodec.gen[TransactionResponse]
  }
}
