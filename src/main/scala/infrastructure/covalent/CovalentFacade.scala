package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.{EthBlockchainRepo, Transaction}
import domain.model.WalletAddress
import infrastructure.covalent.dto.TransactionQueryResponse

import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.json._
import zio.logging.{Logger, Logging}
import zio.stream.ZStream
import zio.{Chunk, Has, IO, Ref, Task, UIO, URLayer, ZIO, ZManaged}

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
                           .map(_.fold[List[Transaction]](_ => List.empty, response => response.data.items))
                           .mapError(err => new RuntimeException("booboo"))
      transactions <- ZIO.foreach(slimTransactions.map(_.hash))(fetchTransaction)
    } yield transactions

  def transactionsStream(address: WalletAddress) = {
    ZStream {
      for {
        stateRef <- Ref.make(1).toManaged_
        pull = stateRef.get.flatMap { pageNumber =>
          if(pageNumber > 0) {
            executeRequest(s"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&page-number=$pageNumber&page-size=20")
              .mapError(Some(_))
              .flatMap(txResponse => {
                if(txResponse.pagination.hasMore) {
                  stateRef.set(pageNumber + 1) *> UIO(Chunk.fromIterable(txResponse.items))
                } else {
                  stateRef.set(0) *> UIO(Chunk.fromIterable(txResponse.items))
                }
              })
          } else {
            IO.fail(None)
          }
        }
      } yield pull
    }
  }

  // Refined Url
  def executeRequest(url: String) =
    httpClient
      .send(
        basicRequest
//          .get(uri"${url.value}")
          .get(uri"$url")
          .response(asString)
      )
      .tapError(t => logger.warn(t.getMessage))
      .mapError(err => err.getMessage)
      .flatMap(response => ZIO.fromEither(response.body.map(_.fromJson[TransactionQueryResponse])))
      .flatMap(either => either.fold(m => ZIO.fail(m), response => {
        if(response.error) {
          ZIO.fail("error")
        } else {
          UIO(response.data)
        }
      }))

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
      transaction <- ZIO.fromEither(body).mapError(err => new RuntimeException("noo")).map(_.data.items.head)
    } yield transaction
}

object CovalentFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[EthBlockchainRepo]] =
    (CovalentFacade(_, _, _)).toLayer
}
