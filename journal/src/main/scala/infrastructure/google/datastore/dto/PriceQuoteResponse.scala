package io.softwarechain.cryptojournal
package infrastructure.google.datastore.dto

import zio.json.{DeriveJsonDecoder, JsonDecoder, jsonField}

object dto {
  final case class PriceQuoteResponse(
    data: PriceQuoteData,
    error: Boolean,
    @jsonField("error_message") errorMessage: Option[String],
    @jsonField("error_code") errorCode: Option[Int]
  )

  final case class PriceQuoteData(
    @jsonField("contract_decimals") contractDecimals: Int,
    @jsonField("contract_name") contractName: String,
    @jsonField("contract_ticker_symbol") contractTickerSymbol: String,
    @jsonField("contract_address") contractAddress: String,
    @jsonField("supports_erc") supportsErc: List[String],
    @jsonField("logo_url") logoUrl: String,
    @jsonField("update_at") updateAt: String,
    @jsonField("quote_currency") quoteCurrency: String,
    @jsonField("prices") prices: List[PriceData]
  )

  final case class PriceData(
    @jsonField("contract_metadata") contractMetadata: ContractMetadata,
    date: String,
    price: Float
  )

  final case class ContractMetadata(
    @jsonField("contract_decimals") contractDecimals: Int,
    @jsonField("contract_name") contractName: String,
    @jsonField("contract_ticker_symbol") contractTickerSymbol: String,
    @jsonField("contract_address") contractAddress: String,
    @jsonField("supports_erc") supportsErc: List[String],
    @jsonField("logo_url") logoUrl: String
  )

  implicit val contractDecoder: JsonDecoder[ContractMetadata] = DeriveJsonDecoder.gen[ContractMetadata]
  implicit val priceDecoder: JsonDecoder[PriceData] = DeriveJsonDecoder.gen[PriceData]
  implicit val dataDecoder: JsonDecoder[PriceQuoteData] = DeriveJsonDecoder.gen[PriceQuoteData]
  implicit val decoder: JsonDecoder[PriceQuoteResponse] = DeriveJsonDecoder.gen[PriceQuoteResponse]
}
