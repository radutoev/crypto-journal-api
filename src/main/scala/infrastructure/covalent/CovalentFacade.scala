package io.softwarechain.cryptojournal
package infrastructure.covalent

import infrastructure.covalent.model.Transaction

import zhttp.http.{HttpData, Method, Request, URL}
import zhttp.http.Status.OK
import zhttp.service
import zhttp.service.Client
import zio.json._
import zio.logging.{Logger, Logging}
import zio.{Has, Task, UIO, URLayer, ZIO}

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
final case class CovalentFacade(httpClient: service.Client,
                                config: CovalentConfig,
                                logger: Logger[String]) extends BlockchainQuery {
  override def fetchTransactions(walletAddress: String): Task[List[Transaction]] =
    (for {
      _ <- logger.info(s"Fetching transactions for $walletAddress")
      url <- ZIO.fromEither(URL.fromString(s"${config.baseUrl}/56/address/$walletAddress/transactions_v2/?key=${config.key}"))
        .mapError(_ => new RuntimeException("booboo"))
      request = Request(endpoint = (Method.GET, url))
      result <- httpClient.request(request)
      transactions <- result.status match {
        case OK => result.content match {
          case HttpData.Empty => UIO(List.empty)
          case HttpData.CompleteData(data) => ZIO.fromEither(data.map(_.toChar).mkString.fromJson[List[Transaction]]).mapError(_ => new RuntimeException("booboo"))
          case HttpData.StreamData(data) => UIO(List.empty)
        }
        case _  => logger.warn(s"Unable to retrieve transactions for $walletAddress") *> UIO(List.empty)
      }
    } yield transactions)

  override def fetchTransaction(txHash: String): Task[List[Transaction]] = ???
}

object CovalentFacade {
  val layer: URLayer[Has[Client] with Has[CovalentConfig] with Logging, Has[BlockchainQuery]] = (CovalentFacade(_, _, _)).toLayer
}
