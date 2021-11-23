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

  //TODO I think I need to return the last day, not the interval here.
  private def currencyTrend(marketPlays: MarketPlays): List[Map[Currency, (TimeInterval, FungibleData)]] = {
    marketPlays.interval match {
      case Some(timeInterval) =>
        timeInterval.days().map { day =>
          val interval = TimeInterval(timeInterval.start.atBeginningOfDay(), day.atEndOfDay())
          val intervalPlays = marketPlays.filter(interval)
          val currencyBalance: CurrencyBalance = new CurrencyBalance(mutable.Map(WBNB -> BigDecimal(0)))
          intervalPlays.plays.foreach {
            case Position(entries, _, _, _) => entries.foreach {
              case a: AirDrop =>
                currencyBalance.subtract(WBNB, a.fee.amount)
                currencyBalance.add(a.received.currency, a.received.amount)
              case a: Approval =>
                currencyBalance.subtract(WBNB, a.fee.amount)
              case buy: Buy =>
                currencyBalance.subtract(WBNB, buy.fee.amount)
                if(buy.spentOriginal.isDefined) {
                  currencyBalance.subtract(buy.spentOriginal.get.currency, buy.spentOriginal.get.amount)
                } else {
                  currencyBalance.subtract(buy.spent.currency, buy.spent.amount)
                }
                currencyBalance.add(buy.received.currency, buy.received.amount)
              case c: Claim =>
                currencyBalance.subtract(WBNB, c.fee.amount)
                currencyBalance.add(c.received.currency, c.received.amount)
              case c: Contribute =>
                currencyBalance.subtract(WBNB, c.fee.amount)
                currencyBalance.subtract(c.spent.currency, c.spent.amount)
              case s: Sell =>
                currencyBalance.subtract(WBNB, s.fee.amount)
                currencyBalance.subtract(s.sold.currency, s.sold.amount)
                currencyBalance.add(s.received.currency, s.received.amount)
              case tIn: TransferIn =>
                currencyBalance.subtract(WBNB, tIn.fee.amount)
                currencyBalance.add(tIn.value.currency, tIn.value.amount)
              case tOut: TransferOut =>
                currencyBalance.subtract(WBNB, tOut.fee.amount)
                currencyBalance.subtract(tOut.amount.currency, tOut.amount.amount)
            }
            case TopUp(_, value, fee, _, _, _) =>
              currencyBalance.subtract(WBNB, fee.amount)
              currencyBalance.add(value.currency, value.amount)
            case Withdraw(_, value, fee, _, _, _) =>
              currencyBalance.subtract(WBNB, fee.amount)
              currencyBalance.subtract(value.currency, value.amount)
          }
          currencyBalance.map.map { case (currency, amount) => currency -> (interval, FungibleData(amount, currency)) }.toMap
        }
      case None => List.empty
    }
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
