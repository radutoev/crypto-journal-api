package io.softwarechain.cryptojournal
package infrastructure.bitquery

import domain.model.WalletAddress
import infrastrucutre.bitquery.graphql.client.Ethereum.dexTrades
import infrastrucutre.bitquery.graphql.client.EthereumDexTrades.{ quotePrice, timeInterval }
import infrastrucutre.bitquery.graphql.client.PriceAggregateFunction.average
import infrastrucutre.bitquery.graphql.client.Query.ethereum
import infrastrucutre.bitquery.graphql.client.TimeInterval.minute
import infrastrucutre.bitquery.graphql.client.{
  DateSelector,
  EthereumCurrencySelector,
  EthereumNetwork,
  StringSelector
}

import sttp.client3._
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio.UIO

//final case class BitQueryFacade () {
//  def getPrices(base: WalletAddress, quote: WalletAddress) = {
//    HttpClientZioBackend().flatMap { backend =>
//      Query.ethereum(network = Some(EthereumNetwork.bsc))(
//        dexTrades(
//          date = Some(DateSelector(since = Some("2021-08-01"))),
//          exchangeName = Some(List(StringSelector(is = Some("Pancake")), StringSelector(is = Some("Pancake v2")))),
//          baseCurrency = Some(List(EthereumCurrencySelector(is = Some(base.value)))),
//          quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(quote.value))))
//        ) {
//          timeInterval {
//            minute(count = Some(1), format = Some("%FT%TZ"))
//          } ~
//            quotePrice(calculate = Some(average))
//        }).toRequest(uri"https://graphql.bitquery.io/")
//        .headers(Map("X-API-KEY" -> "BQYVTE222H3jH2ZnySdrpYGtBsUsGC8N"))
//        .send(backend)
//        .map(_.body)
//    }
//  }
//}

object BitQueryFacade {
  def getPrices(base: WalletAddress, quote: WalletAddress) = {
    val query = ethereum(network = Some(EthereumNetwork.bsc))(
      dexTrades(
        date = Some(DateSelector(since = Some("2021-08-01"))),
//          exchangeName = Some(List(StringSelector(is = Some("Pancake")), StringSelector(is = Some("Pancake v2")))),
        exchangeName = Some(List(StringSelector(is = Some("Pancake V2")))),
        baseCurrency = Some(List(EthereumCurrencySelector(is = Some(base.value)))),
        quoteCurrency = Some(List(EthereumCurrencySelector(is = Some(quote.value))))
      ) {
        timeInterval {
          minute(count = Some(1), format = Some("%FT%TZ"))
        } ~
        quotePrice(calculate = Some(average))
      }
    )

    HttpClientZioBackend().flatMap { backend =>
      query
        .toRequest(uri"https://graphql.bitquery.io/", dropNullInputValues = true, useVariables = true)
        .headers(Map("X-API-KEY" -> "BQYVTE222H3jH2ZnySdrpYGtBsUsGC8N"))
        .send(backend)
        .map(_.body)
        .absolve
        .tap(res => UIO(println(res)))
    }
  }
}
