package io.softwarechain.cryptojournal
package infrastructure.covalent

import infrastructure.covalent.model.{Transaction, TransactionQueryResponse, TransactionResponse}

import sttp.client3.httpclient.zio.SttpClient
import sttp.client3._
import zio.json._
import zio.logging.{Logger, Logging}
import zio.{Has, Task, URLayer, ZIO}

//TODO This should be in the domain, and be named something else.
trait BlockchainQuery {
  def fetchTransactions(walletAddress: String): Task[List[Transaction]]

  def fetchTransaction(txHash: String): Task[List[Transaction]]
}

object BlockchainQuery {
  def fetchTransactions(walletAddress: String): ZIO[Has[BlockchainQuery], Throwable, List[Transaction]] =
    ZIO.serviceWith[BlockchainQuery](_.fetchTransactions(walletAddress))

  def fetchTransaction(txHash: String): ZIO[Has[BlockchainQuery], Throwable, List[Transaction]] =
    ZIO.serviceWith[BlockchainQuery](_.fetchTransaction(txHash))
}

//
final case class CovalentFacade(httpClient: SttpClient.Service,
                                config: CovalentConfig,
                                logger: Logger[String]) extends BlockchainQuery {
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
        .send(basicRequest.get(uri"${config.baseUrl}/56/address/$walletAddress/transactions_v2/?key=${config.key}")
        .response(asString))
      //TODO Better handling of response.
      transactions <- ZIO.fromEither(response.body)
        .map(_.fromJson[TransactionQueryResponse])
        .map(_.fold[List[Transaction]](_ => List.empty, response => response.data.items))
        .mapError(_ => new RuntimeException("booboo"))
    } yield transactions

  override def fetchTransaction(txHash: String): Task[List[Transaction]] = ???
}

object CovalentFacade {
  val layer: URLayer[SttpClient with Has[CovalentConfig] with Logging, Has[BlockchainQuery]] = (CovalentFacade(_, _, _)).toLayer
}
