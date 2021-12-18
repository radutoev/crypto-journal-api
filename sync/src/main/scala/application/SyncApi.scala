package io.softwarechain.cryptojournal
package application

import domain.model.{TransactionHash, WalletAddress}
import domain.pricequote.PriceQuotesJobService
import domain.wallet.{WalletCache, WalletRepo, error}
import infrastructure.binance.TradingStream
import infrastructure.google.datastore.DatastorePaginationRepo
import util.ListOps.cond

import io.softwarechain.cryptojournal.domain.blockchain.BlockchainRepo
import io.softwarechain.cryptojournal.domain.position.MarketPlayRepo
import io.softwarechain.cryptojournal.domain.wallet.error.WalletError
import org.web3j.protocol.core.methods.response.EthBlock.TransactionObject
import org.web3j.protocol.core.methods.response.Transaction
import zio.clock.Clock
import zio.duration.durationInt
import zio.stream.ZStream
import zio.{Has, Schedule, URIO, ZIO}

import scala.jdk.CollectionConverters.CollectionHasAsScala

object SyncApi {
  def updatePriceQuotes(): URIO[Has[PriceQuotesJobService] with Clock, Unit] =
    (ZIO.serviceWith[PriceQuotesJobService](_.updateQuotes()) repeat Schedule.spaced(30.seconds)).unit.ignore

  def clearPaginationContext(): URIO[Has[DatastorePaginationRepo] with Clock, Unit] =
    (ZIO.serviceWith[DatastorePaginationRepo](_.cleanup()) repeat Schedule.spaced(1.day)).unit.ignore

//  def updatePositions(): ZStream[Has[WalletCache], Throwable, Transaction] = {
//    TradingStream.bscStream()
//      .map(_.getBlock.getTransactions.asScala.toList.map(_.get().asInstanceOf[TransactionObject]))
//      .flattenIterables
//      .map(txResponse => txResponse.get())
//      .mapM(tx => {
//        val from = WalletAddress.unsafeFrom(tx.getFrom)
//        val to   = WalletAddress.unsafeFrom(tx.getTo)
//        for {
//          fromFound <- WalletCache.exists(from)
//          toFound   <- WalletCache.exists(to)
//          addresses = Nil ++ cond(fromFound, () => from) ++ cond(toFound, () => to)
//        } yield (TransactionHash.unsafeApply(tx.getHash), addresses)
//      })
//      .filter { case (_, addresses) => addresses.nonEmpty }
////      .mapM { case (hash, _) => BlockchainRepo.fetchTransaction(hash).map(tx => ) }
//      .forever
//  }

  def loadWallets(): ZIO[Has[WalletRepo] with Has[WalletCache], WalletError, Unit] = {
    ZIO.services[WalletRepo, WalletCache].flatMap { case (walletRepo, walletCache) =>
      walletRepo.getWallets().flatMap(wallets => walletCache.addWallets(wallets.toSet))
    }
  }
}
