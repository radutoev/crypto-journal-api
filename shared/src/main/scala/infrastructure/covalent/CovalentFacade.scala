package io.softwarechain.cryptojournal
package infrastructure.covalent

import config.CovalentConfig
import domain.blockchain.error.{BlockchainError, HistoricalPriceGetError, TransactionsGetError}
import domain.blockchain.{BlockchainRepo, Transaction}
import domain.model.{Currency, TransactionHash, WalletAddress}
import domain.pricequote.PriceQuote
import infrastructure.covalent.CovalentFacade.CovalentQParamDateFormat
import infrastructure.covalent.dto.{PriceQuoteResponse, TransactionQueryResponse}
import vo.TimeInterval

import eu.timepit.refined.api.Refined
import eu.timepit.refined.refineV
import eu.timepit.refined.string.Url
import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.json._
import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Chunk, Has, IO, Ref, Task, UIO, URLayer, ZIO}

import java.time.format.DateTimeFormatter
import java.time.{Instant, LocalDate, ZoneId, ZoneOffset}

final case class CovalentFacade(httpClient: SttpClient.Service, config: CovalentConfig, logger: Logger[String])
    extends BlockchainRepo {
  //I have a limit at the moment because I use this only for the demo import functionality.
  override def fetchTransactions(address: WalletAddress): Task[List[Transaction]] =
    for {
      _   <- logger.info(s"Fetching transactions for $address")
      url = s"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&page-number=1&page-size=${config.pageSize}"
      response <- httpClient
                   .send(
                     basicRequest
                       .get(uri"$url")
                       .response(asString)
                   )
                   .tapError(t => logger.warn(t.getMessage))
      //TODO Better handling of response.
      slimTransactions <- ZIO
                           .fromEither(response.body)
                           .tapError(s => logger.warn(s))
                           .map(_.fromJson[TransactionQueryResponse])
                           .mapBoth(
                             err => new RuntimeException("booboo"),
                             _.fold[List[Transaction]](
                               _ => List.empty,
                               response => response.data.items.map(_.toDomain())
                             )
                           )
      transactions <- ZIO.foreach(slimTransactions.map(tx => tx.hash))(fetchTransaction)
    } yield transactions

  def transactionsStream(address: WalletAddress): ZStream[Any, TransactionsGetError, Transaction] =
    txStream(address, _ => true)

  override def transactionsStream(
    address: WalletAddress,
    startFrom: Instant
  ): ZStream[Any, TransactionsGetError, Transaction] =
    txStream(address, tx => tx.instant.compareTo(startFrom) > 0)

  private def txStream(
    address: WalletAddress,
    predicate: Transaction => Boolean
  ): ZStream[Any, TransactionsGetError, Transaction] =
    ZStream {
      for {
        stateRef <- Ref.make(1).toManaged_
        pull = stateRef.get.flatMap { pageNumber =>
          if (pageNumber > 0) {
            executeRequest(
              refineV[Url].unsafeFrom(
                s"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&page-number=$pageNumber&page-size=${config.pageSize}"
              )
            ).tapError(err => logger.warn("Covalent request failed: " + err.message))
              .mapError(Some(_))
              .flatMap { txResponse =>
                val items = txResponse.items.map(_.toDomain()).filter(predicate)
                if (txResponse.pagination.isDefined && txResponse.pagination.get.hasMore && items.nonEmpty) {
                  stateRef.set(pageNumber + 1) *> UIO(Chunk.fromIterable(items))
                } else {
                  stateRef.set(0) *> UIO(Chunk.fromIterable(items))
                }
              }
          } else {
            IO.fail(None)
          }
        }
      } yield pull
    }

  private def executeRequest(url: String Refined Url) =
    logger.info(s"Fetching data from ${url.value}") *>
      httpClient
        .send(
          basicRequest
            .get(uri"${url.value}")
            .response(asString)
        )
        .mapError(err => TransactionsGetError(err.getMessage))
        .retryN(2)
        .tapError(err => logger.warn(s"Transactions fetch failed for ${url.value}. Error: ${err}"))
        .flatMap(response =>
          ZIO
            .fromEither(response.body.map(_.fromJson[TransactionQueryResponse]))
            .tapError(err => logger.warn(s"Decode failed for ${response.body}. Error: $err"))
            .mapError(TransactionsGetError)
        )
        .flatMap(either =>
          either.fold(
            m => ZIO.fail(TransactionsGetError(m)),
            response =>
              if (response.error) {
                ZIO.fail(TransactionsGetError(s"Failure fetching transactions: ${response.errorCode}"))
              } else {
                UIO(response.data)
              }
          )
        )

  override def fetchTransaction(txHash: TransactionHash): Task[Transaction] =
    for {
      _   <- logger.info(s"Fetching transaction by hash ${txHash.value}")
      url = s"${config.baseUrl}/56/transaction_v2/${txHash.value}/?key=${config.key}"
      response <- httpClient
                   .send(
                     basicRequest
                       .get(uri"$url")
                       .response(asString)
                   )
                   .tapError(t => logger.warn(t.getMessage))
      body <- ZIO
               .fromEither(response.body)
               .mapBoth(err => new RuntimeException(err), _.fromJson[TransactionQueryResponse])
      transaction <- ZIO
                      .fromEither(body)
                      .tapError(t => logger.warn(t))
                      .mapBoth(err => new RuntimeException(err), _.data.items.head.toDomain())
    } yield transaction

  override def getHistoricalPriceQuotes(
    currency: Currency,
    interval: TimeInterval
  ): IO[BlockchainError, List[PriceQuote]] =
    for {
      _     <- logger.info(s"Get historical price quotes for ${currency.value} in interval $interval")
      start = CovalentQParamDateFormat.format(interval.start)
      end   = CovalentQParamDateFormat.format(interval.end)
      url   = s"${config.baseUrl}/pricing/historical_by_addresses_v2/56/USD/${currency.value}/?from=$start&to=$end&key=${config.key}"
      _     <- logger.debug(url)
      response <- httpClient
                   .send(basicRequest.get(uri"$url").response(asString))
                   .tapError(t => logger.warn(s"Covalent price quote request failed: $t"))
                   .mapError(t => HistoricalPriceGetError(t.getMessage))
      decoded <- ZIO
                  .fromEither(response.body)
                  .tapError(err => logger.warn(err))
                  .mapError(err => HistoricalPriceGetError(err))
                  .flatMap(r =>
                    ZIO
                      .fromEither(r.fromJson[PriceQuoteResponse])
                      .tapError(err => logger.warn(err))
                      .mapError(err => HistoricalPriceGetError(err))
                  )
      _ <- logger.debug(s"Data for ${currency.value}: $decoded")
      quotes = decoded.data.flatMap(priceData =>
        priceData.prices.map(price =>
          PriceQuote(price.price, LocalDate.parse(price.date).atStartOfDay().toInstant(ZoneOffset.UTC))
        )
      )
    } yield quotes
}

object CovalentFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[BlockchainRepo]] =
    (CovalentFacade(_, _, _)).toLayer

  private[covalent] val CovalentQParamDateFormat =
    DateTimeFormatter.ofPattern("yyyy-MM-dd").withZone(ZoneId.from(ZoneOffset.UTC))
}
