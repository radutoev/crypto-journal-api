package io.softwarechain.cryptojournal
package infrastructure.covalent


import domain.blockchain.{EthBlockchainRepo, Transaction}

import io.softwarechain.cryptojournal.infrastructure.covalent.dto.TransactionQueryResponse
import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.json._
import zio.logging.{Logger, Logging}
import zio.{Has, Task, URLayer, ZIO}

final case class CovalentFacade(httpClient: SttpClient.Service, config: CovalentConfig, logger: Logger[String])
    extends EthBlockchainRepo {
  override def fetchTransactions(walletAddress: String): Task[List[Transaction]] =
    for {
      _ <- logger.info(s"Fetching transactions for $walletAddress")
      //      url <- ZIO.fromEither(URL.fromString(s"${config.baseUrl}/56/address/$walletAddress/transactions_v2/?key=${config.key}"))
      //        .mapError(_ => new RuntimeException("booboo"))
      //      request = Request(endpoint = (Method.GET, url))
      //      result <- httpClient.request(request)
      //      transactions <- result.status match {
      //        case OK => result.content match {
      //          case HttpData.Empty => UIO(List.empty)
      //          case HttpData.CompleteData(data) => ZIO.fromEither(data.map(_.toChar).mkString.fromJson[List[Transaction]]).mapError(_ => new RuntimeException("booboo"))
      //          case HttpData.StreamData(data) => UIO(List.empty)
      //        }
      //        case _  => logger.warn(s"Unable to retrieve transactions for $walletAddress") *> UIO(List.empty)
      //      }
      response <- httpClient
                   .send(
                     basicRequest
                       .get(uri"${config.baseUrl}/56/address/$walletAddress/transactions_v2/?key=${config.key}")
                       .response(asString)
                   )
      //TODO Better handling of response.
      slimTransactions <- ZIO
                       .fromEither(response.body)
                       .map(_.fromJson[TransactionQueryResponse])
                       .map(_.fold[List[Transaction]](err => List.empty, response => response.data.items))
                       .mapError(_ => new RuntimeException("booboo"))
      transactions <- ZIO.foreach(slimTransactions.map(_.hash))(fetchTransaction)
    } yield transactions

  override def fetchTransaction(txHash: String): Task[Transaction] =
    for {
      _ <- logger.info(s"Fetching transaction $txHash")
      response <- httpClient.send(
                   basicRequest
                     .get(uri"${config.baseUrl}/56/transaction_v2/$txHash/?key=${config.key}")
                     .response(asString)
                 )
      body        <- ZIO.fromEither(response.body).map(_.fromJson[TransactionQueryResponse]).mapError(err => new RuntimeException("noo"))
      transaction <- ZIO.fromEither(body).mapError(err => new RuntimeException("noo")).map(_.data.items.head)
    } yield transaction
}

object CovalentFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[EthBlockchainRepo]] =
    (CovalentFacade(_, _, _)).toLayer
}
