package io.softwarechain.cryptojournal
package application

import domain.blockchain.BlockchainRepo
import domain.model._
import domain.model.date.{Hour, TimeUnit}
import domain.position.PositionEntry
import domain.pricequote.{CurrencyAddressPair, CurrencyPair, PriceQuote, PriceQuoteRepo}
import infrastructure.bitquery.BitQueryFacade
import vo.TimeInterval

import eu.timepit.refined.refineMV
import io.softwarechain.cryptojournal.domain.portfolio.{StatsService, error}
import io.softwarechain.cryptojournal.domain.portfolio.model.PlaysGrouping
import io.softwarechain.cryptojournal.domain.wallet.Wallet
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
      _.getPrices(
        CurrencyAddressPair(
          CurrencyAddress(
            WBNB,
            CoinAddress.unsafeFrom("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c")
          ),
          CurrencyAddress(
            BUSD,
            CoinAddress.unsafeFrom("0xe9e7cea3dedca5984780bafc599bd69add087d56")
          )
        ),
        Hour(Instant.parse("2021-10-04T00:00:00.000Z"))
      )
    )

  def quotesTest(
    pair: CurrencyPair,
    interval: TimeInterval,
    unit: TimeUnit
  ): ZIO[Has[PriceQuoteRepo], Throwable, List[PriceQuote]] =
    ZIO.serviceWith(_.getQuotes(pair, interval, unit).orElseFail(new RuntimeException("failed")))

  def aggregationDetails(address: WalletAddress, interval: TimeInterval, groupings: Set[PlaysGrouping]): ZIO[Has[StatsService], RuntimeException, String] =
    ZIO.serviceWith[StatsService](_.playsDistribution(
      Wallet(address = address, userId = refineMV( "dummy-user")),
      interval,
      groupings,
      withSourceData = true
    ).map(_.toList.head._2).map { bins =>
      "Name,Total Net Return,Total Return Percentage,Position Return,Position Return Percentage,ID\n" +
        bins.map { bin =>
          val common = List(bin._1.value,
            bin._2.binData.tradeCount.value,
            bin._2.binData.netReturn.amount + " " + bin._2.binData.netReturn.currency,
            bin._2.binData.returnPercentage
          ).mkString(",")

          bin._2.source.map(p =>
            common + "," + List(
              p.fiatReturn.getOrElse(FungibleData.zero(BUSD).amount),
              p.fiatReturnPercentage.getOrElse(BigDecimal(0)),
              p.id.get.value
            ).mkString(",") + "\n"
          ).mkString("")
        }.mkString("")
    }.mapError(e => new RuntimeException(e.toString))
  )
}
