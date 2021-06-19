package io.softwarechain.cryptojournal
package infrastructure.covalent

import domain.blockchain.{ EthBlockchainRepo, Transaction }
import domain.model.WalletAddress
import infrastructure.covalent.dto.TransactionQueryResponse

import sttp.client3._
import sttp.client3.httpclient.zio.SttpClient
import zio.json._
import zio.logging.{ Logger, Logging }
import zio.{ Has, Task, URLayer, ZIO }

final case class CovalentFacade(httpClient: SttpClient.Service, config: CovalentConfig, logger: Logger[String])
    extends EthBlockchainRepo {
  //I have a limit at the moment because I use this only for the demo import functionality.
  override def fetchTransactions(address: WalletAddress): Task[List[Transaction]] =
    for {
      _ <- logger.info(s"Fetching transactions for $address")
      response <- httpClient
                   .send(
                     basicRequest
                       .get(uri"${config.baseUrl}/56/address/${address.value}/transactions_v2/?key=${config.key}&limit=${config.demoTxCount}")
                       .response(asString)
                   )
      //TODO Better handling of response.
      slimTransactions <- ZIO
                           .fromEither(response.body)
                           .map(_.fromJson[TransactionQueryResponse])
                           .map(_.fold[List[Transaction]](_ => List.empty, response => response.data.items))
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
