package io.softwarechain.cryptojournal
package infrastructure.covalent

import config.CovalentConfig
import domain.model.{Currency, FungibleData, USDT, WalletAddress}
import domain.wallet.WalletRepo
import domain.wallet.error.{BalanceGetError, WalletError}
import infrastructure.covalent.dto.AccountBalanceResponse

import sttp.client3.httpclient.zio.SttpClient
import sttp.client3.{UriContext, asString, basicRequest}
import zio.json.DecoderOps
import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer, ZIO}

import scala.concurrent.duration.DurationInt

final case class CovalentWalletRepo(httpClient: SttpClient.Service,
                                    covalentConfig: CovalentConfig,
                                    logger: Logger[String]) extends WalletRepo {
  override def getQuote(address: WalletAddress, coin: Currency): IO[WalletError, FungibleData] =
    for {
      _ <- logger.info(s"Fetching ${coin.value} quote for ${address.value}")
      matchQuery = s"""{contract_ticker_symbol: ${coin.value} }"""
      url = s"${covalentConfig.baseUrl}/56/address/${address.value}/balances_v2/?key=${covalentConfig.key}&match=$matchQuery&quote-currency=USDT"
      response <- httpClient
        .send(basicRequest.readTimeout(2.minutes).get(uri"$url").response(asString))
        .tapError(t => logger.warn(s"Covalent address balance request failed: ${t.getMessage}"))
        .mapError(t => BalanceGetError(t.getMessage))
      decoded <- ZIO.fromEither(response.body).mapError(BalanceGetError)
        .flatMap(r => ZIO.fromEither(r.fromJson[AccountBalanceResponse])
        .tapError(err => logger.warn(s"$err on URL: $url"))
        .mapError(BalanceGetError))
      quote <- ZIO.fromOption(decoded.data.items.headOption)
        .mapBoth(_ => BalanceGetError("Invalid response"), balance => balance.quote)
    } yield FungibleData(BigDecimal(quote), USDT)
}

object CovalentWalletRepo {
  lazy val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[WalletRepo]] =
    (CovalentWalletRepo(_, _, _)).toLayer
}
