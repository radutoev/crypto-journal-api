package io.softwarechain.cryptojournal
package application

import domain.blockchain.BlockchainRepo
import domain.model._
import domain.model.date.Hour
import domain.position.PositionEntry
import domain.pricequote.{CurrencyAddressPair, PriceQuote}
import infrastructure.bitquery.BitQueryFacade

import zio.{Has, UIO, ZIO}

import java.time.Instant

object PositionHelper {
  def txToEntries(
    address: WalletAddress,
    txHash: TransactionHash
  ): ZIO[Has[BlockchainRepo], Throwable, List[PositionEntry]] =
    ZIO.serviceWith(
      _.fetchTransaction(txHash)
        .flatMap(tx =>
          ZIO
            .fromEither(PositionEntry.fromTransaction(tx, address))
            .tapError(err => UIO(println(err)))
            .mapError(new RuntimeException(_))
        )
    )

  def bitqueryTest(): ZIO[Has[BitQueryFacade], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(
      _.getPrices(CurrencyAddressPair(
        CurrencyAddress(
          WBNB,
          CoinAddress.unsafeFrom("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c")
        ),
        CurrencyAddress(
          BUSD,
          CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56")
        )
      ), Hour(Instant.parse("2021-10-04T00:00:00.000Z")))
    )
}
