package io.softwarechain.cryptojournal
package application

import domain.blockchain.{BlockchainRepo, Transaction}
import domain.model.{TransactionHash, WalletAddress, WalletAddressPredicate}
import domain.position.{MarketPlayRepo, MarketPlays}
import domain.pricequote.PriceQuotesJobService
import domain.wallet.error.WalletError
import domain.wallet.{WalletCache, WalletRepo}
import infrastructure.binance.TradingStream
import infrastructure.google.datastore.DatastorePaginationRepo

import eu.timepit.refined.refineV
import org.web3j.protocol.core.methods.response.EthBlock.TransactionObject
import zio.clock.Clock
import zio.duration.durationInt
import zio.logging.Logging
import zio.{Has, Schedule, UIO, URIO, ZIO}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object SyncApi {
  def updatePriceQuotes(): URIO[Has[PriceQuotesJobService] with Clock, Unit] =
    (ZIO.serviceWith[PriceQuotesJobService](_.updateQuotes()) repeat Schedule.spaced(1.day)).unit.ignore

  def clearPaginationContext(): URIO[Has[DatastorePaginationRepo] with Clock, Unit] =
    (ZIO.serviceWith[DatastorePaginationRepo](_.cleanup()) repeat Schedule.spaced(1.day)).unit.ignore

  def updatePositions(): ZIO[Has[MarketPlayRepo] with Logging with Has[BlockchainRepo] with Has[WalletCache], Throwable, Nothing] = {
    TradingStream.bscStream()
      .tapError(t => Logging.warn(t.getMessage))
      .map { block =>
        Option(block.getBlock)
          .flatMap(b => Option(b.getTransactions))
          .map(_.asScala.toList)
          .getOrElse(List.empty)
          .map(_.get().asInstanceOf[TransactionObject])
      }
      .flattenIterables
      .map(txResponse => txResponse.get())
      .tap(tx => Logging.debug(s"Check transaction ${tx.getHash}"))
      .mapM(tx => {
        val txHash = TransactionHash.unsafeApply(tx.getHash)

        def forAddress(rawAddress: String) = {
          (for {
            address <- ZIO.fromOption(Option(rawAddress))
            wallet  <- ZIO.fromEither(refineV[WalletAddressPredicate](address))
            found   <- UIO(true) //WalletCache.exists(wallet)
          } yield if(found) List(wallet) else Nil).orElse(UIO(Nil))
        }

        for {
          from      <- forAddress(tx.getFrom)
          to        <- forAddress(tx.getTo)
          addresses = Nil ++ from ++ to
        } yield (txHash, addresses)
      })
      .filter { case (_, addresses) => addresses.nonEmpty }
      .mapM { case (hash, addresses) =>
        Logging.info(s"Processing transaction ${hash.value}") *>
        BlockchainRepo.fetchTransaction(hash)
          .map(tx => interpretTransaction(addresses, tx))
          .tapError(_ => Logging.error(s"Error fetching transaction ${hash.value}"))
          .orElse(UIO(List.empty))
      }
      .flattenIterables
      .foreach { case (address, plays) => MarketPlayRepo.merge(address, plays) }
      .ignore
      .forever
  }

  private def interpretTransaction(addresses: List[WalletAddress], transaction: Transaction): List[(WalletAddress, MarketPlays)] = {
    addresses.map(address => interpretTransaction(address, transaction))
  }

  private def interpretTransaction(address: WalletAddress, transaction: Transaction): (WalletAddress, MarketPlays) = {
    address -> MarketPlays.findMarketPlays(address, List(transaction))
  }

  def loadWallets(): ZIO[Has[WalletRepo] with Has[WalletCache], WalletError, Unit] = {
    ZIO.services[WalletRepo, WalletCache].flatMap { case (walletRepo, walletCache) =>
      walletRepo.getWallets().flatMap(wallets => walletCache.addWallets(wallets.toSet))
    }
  }

  def addWallet(address: WalletAddress): ZIO[Has[WalletCache], Nothing, Unit] = {
    ZIO.serviceWith[WalletCache](_.addWallet(address))
  }
}
