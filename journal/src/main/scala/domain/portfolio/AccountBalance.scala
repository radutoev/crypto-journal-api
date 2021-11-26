package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{Currency, FungibleData, USD, WBNB}
import domain.portfolio.error.{AccountBalanceComputeError, PortfolioError}
import domain.position._
import domain.pricequote.{PriceQuote, PriceQuoteRepo, PriceQuotes}
import domain.wallet.{Wallet, WalletRepo}
import util.InstantOps
import vo.TimeInterval

import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, URLayer}

import scala.collection.mutable

trait AccountBalance {
  def value(wallet: Wallet): IO[PortfolioError, FungibleData]

  def trend(wallet: Wallet): IO[PortfolioError, List[FungibleData]]
}

final case class LiveAccountBalance(
  marketPlaysService: MarketPlayService,
  priceQuoteRepo: PriceQuoteRepo, //TODO Use PriceQuoteService.
  walletRepo: WalletRepo,
  clock: Clock.Service,
  logger: Logger[String]
) extends AccountBalance {
  override def value(wallet: Wallet): IO[PortfolioError, FungibleData] =
    for {
      _            <- logger.info(s"Computing account balance for $wallet")
      marketPlays  <- marketPlaysService.getPlays(wallet).orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
      distribution = marketPlays.distributionByDay()
      _            <- logger.info("Computed distribution")
//      trend = currencyTrend(marketPlays)

//      _ = trend.foreach(map => map.get(Currency.unsafeFrom("WBNB")) match {
//        case Some(value) => println(value._1 + "  " + value._2.amount)
//        case None => println()
//      })

//      currencyQuotes <- priceQuoteRepo.getQuotes(marketPlays.currencies.map(_._1), marketPlays.interval.get)
//        .orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
//
//      _ <- logger.info("Finished processing")
//
//      balance = trend.last.map { case (currency, (interval, fungibleData)) =>
//        val quotes = PriceQuotes(currencyQuotes.getOrElse(currency, List.empty))
//        quotes.findPrice(interval.start.atBeginningOfDay())
//          .map(quote => {
//            println(fungibleData + " at " + quote.price + " totaling " + (quote.price * fungibleData.amount))
//            quote.price * fungibleData.amount
//          })
//          .getOrElse(BigDecimal(0))
//      }.sum
    } yield FungibleData(0, USD)

  override def trend(wallet: Wallet): IO[PortfolioError, List[FungibleData]] = {
    for {
      _           <- logger.info(s"Computing account balance trend for $wallet")
      marketPlays <- marketPlaysService.getPlays(wallet).orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
    } yield List.empty
  }
}

private class CurrencyBalance(val map: mutable.Map[Currency, BigDecimal]) {
  def add(currency: Currency, amount: BigDecimal): Unit = {
//    println(s"+$amount $currency")
    map.update(currency, map.getOrElse(currency, BigDecimal(0)) + amount)
  }

  def subtract(currency: Currency, amount: BigDecimal): Unit = {
//    println(s"-$amount $currency")
    map.update(currency, map.getOrElse(currency, BigDecimal(0)) - amount)
  }
}

object LiveAccountBalance {
  lazy val layer
    : URLayer[Has[MarketPlayService] with Has[PriceQuoteRepo] with Has[WalletRepo] with Clock with Logging, Has[
      AccountBalance
    ]] =
    (LiveAccountBalance(_, _, _, _, _)).toLayer
}
