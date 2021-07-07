package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.{ EthBlockchainRepo, Transaction }
import domain.blockchain.error._
import domain.model.WalletAddress
import infrastructure.covalent.dto.TransactionQueryResponse

import eu.timepit.refined.api.Refined
import eu.timepit.refined.string.Url
import eu.timepit.refined.refineV
import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.json._
import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Chunk, Has, IO, Ref, Task, UIO, URLayer, ZIO }

import java.time.Instant

final case class CovalentFacade(httpClient: SttpClient.Service, config: CovalentConfig, logger: Logger[String])
    extends EthBlockchainRepo {
  //I have a limit at the moment because I use this only for the demo import functionality.
  override def fetchTransactions(address: WalletAddress): Task[List[Transaction]] =
    for {
      _   <- logger.info(s"Fetching transactions for $address")
      url = s"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&page-number=1&page-size=60"
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
                           .map(
                             _.fold[List[Transaction]](
                               _ => List.empty,
                               response => response.data.items.map(_.toDomain())
                             )
                           )
                           .mapError(err => new RuntimeException("booboo"))
      transactions <- ZIO.foreach(slimTransactions.map(_.hash))(fetchTransaction)
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
                s"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&page-number=$pageNumber&page-size=20"
              )
            ).tapError(err => logger.warn("Covalent request failed: " + err.message))
              .mapError(Some(_))
              .flatMap { txResponse =>
                val items = txResponse.items.map(_.toDomain()).filter(predicate)
                if (txResponse.pagination.hasMore && items.nonEmpty) {
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

  // Refined Url
  def executeRequest(url: String Refined Url) =
    httpClient
      .send(
        basicRequest
          .get(uri"${url.value}")
          .response(asString)
      )
      .tapError(t => logger.warn("Covalent request failed: " + t.getMessage))
      .mapError(err => TransactionsGetError(err.getMessage))
      .flatMap(response =>
        ZIO.fromEither(response.body.map(_.fromJson[TransactionQueryResponse])).mapError(TransactionsGetError)
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

  override def fetchTransaction(txHash: String): Task[Transaction] =
    for {
      _   <- logger.info(s"Fetching transaction $txHash")
      url = s"${config.baseUrl}/56/transaction_v2/$txHash/?key=${config.key}"
      response <- httpClient
                   .send(
                     basicRequest
                       .get(uri"$url")
                       .response(asString)
                   )
                   .tapError(t => logger.warn(t.getMessage))
      body <- ZIO
               .fromEither(response.body)
               .map(_.fromJson[TransactionQueryResponse])
               .mapError(err => new RuntimeException("noo"))
      transaction <- ZIO
                      .fromEither(body)
                      .mapError(err => new RuntimeException("noo"))
                      .map(_.data.items.head.toDomain())
    } yield transaction
}

object CovalentFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[EthBlockchainRepo]] =
    (CovalentFacade(_, _, _)).toLayer
}
