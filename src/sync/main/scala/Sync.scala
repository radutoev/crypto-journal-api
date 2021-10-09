package io.softwarechain.cryptojournal

import config.CryptoJournalConfig
import domain.LiveWalletRepo
import domain.blockchain.BlockchainRepo
import domain.model.TransactionHash
import infrastructure.binance.BnbListener
import infrastructure.covalent.CovalentFacade

import com.typesafe.config.{Config, ConfigFactory}
import org.web3j.protocol.core.methods.response.EthBlock.TransactionObject
import org.web3j.protocol.core.methods.response.Transaction
import sttp.client3.httpclient.zio.HttpClientZioBackend
import zio._
import zio.config.typesafe.TypesafeConfig
import zio.logging.slf4j.Slf4jLogger

import scala.jdk.CollectionConverters._

object Sync extends App {
  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    ZIO(ConfigFactory.load.resolve())
      .flatMap(rawConfig => program(rawConfig))
      .exitCode

  private def program(config: Config) =
    for {
      bnbStream <- BnbListener
                    .positionEntryStream()
                    .map(_.getBlock.getTransactions.asScala.toList.map(_.get().asInstanceOf[TransactionObject]))
                    .flattenIterables
                    .filterM(txObject => filterByWalletPresence(txObject.get()))
                    .map(txObject => TransactionHash.unsafeApply(txObject.get().getHash))
                    .mapM(txHash => BlockchainRepo.fetchTransaction(txHash))
                    .foreach(t => UIO(println(t)))
                    .provideCustomLayer(listenerEnvironment(config))
                    .forever
                    .fork
      _ <- bnbStream.join
    } yield ()

  //TODO I think i need to split based on to vs from presence.
  private def filterByWalletPresence(tx: Transaction) =
    UIO(false)
//    if(tx.getTo != null && tx.getFrom != null) {
//      for {
//        toFound   <- WalletRepo.exists(WalletAddress.unsafeApply(tx.getTo))
//        fromFound <- WalletRepo.exists(WalletAddress.unsafeApply(tx.getFrom))
//        _         <- if(toFound || fromFound) {
//          Logging.info(s"True from transaction ${tx.getHash}")
//        } else {
//          Logging.info(s"False from transaction ${tx.getHash}")
//        }
//      } yield toFound || fromFound
//    } else {
//      Logging.info(s"Transaction ${tx.getHash} has not to/from values") *> UIO(false)
//    }

  private def listenerEnvironment(config: Config) = {
    val configLayer         = TypesafeConfig.fromTypesafeConfig(config, CryptoJournalConfig.descriptor)
    val covalentConfigLayer = configLayer.map(c => Has(c.get.covalent))

    lazy val httpClientLayer = HttpClientZioBackend.layer()

    lazy val loggingLayer = {
      val logFormat = "%s"
      Slf4jLogger.make((_, message) => logFormat.format(message))
    }

    lazy val exchangeRepoLayer = (httpClientLayer ++ loggingLayer ++ covalentConfigLayer) >>> CovalentFacade.layer
    lazy val walletRepoLayer   = LiveWalletRepo.layer

    exchangeRepoLayer ++ loggingLayer ++ walletRepoLayer
  }
}
