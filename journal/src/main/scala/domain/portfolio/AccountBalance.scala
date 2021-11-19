package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{Currency, FungibleData, WBNB}
import domain.portfolio.error.{AccountBalanceComputeError, PortfolioError}
import domain.position._
import domain.pricequote.PriceQuoteRepo
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
  priceQuoteRepo: PriceQuoteRepo,
  walletRepo: WalletRepo,
  clock: Clock.Service,
  logger: Logger[String]
) extends AccountBalance {
  override def value(wallet: Wallet): IO[PortfolioError, FungibleData] =
    for {
      _   <- logger.info(s"Computing account balance for $wallet")
      now <- clock.instant
      marketPlays <- marketPlaysService
                      .getPlays(wallet)
                      .orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
      positionsByCoinAddress = marketPlays.plays.collect {
        case p: Position if p.coinAddress.isDefined => p.coinAddress.get -> p
      }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap

      trend = currencyTrend(marketPlays)

      _ <- logger.info("Finished processing")

//      balance <- walletRepo.getQuote(wallet.address, Currency.unsafeFrom("BNB"))
//        .zipWithPar(walletRepo.getQuote(wallet.address, Currency.unsafeFrom("USDT")))((f1, f2) => f1.add(f2.amount))
//        .orElseFail(AccountBalanceComputeError("Cannot compute account balance"))
//        .map(mainQuotes => FungibleData(amount, USD).add(mainQuotes.amount))

//      priceQuoteEffects = positionsByCoinAddress.keySet.map(coinAddress =>
//        priceQuoteRepo
//          .getCurrentQuote(coinAddress)
//          .map(quote => coinAddress -> quote)
////          .catchSome {
////            case PriceQuoteNotFound(contract) => UIO(contract -> PriceQuote(0f, now))
////          }
//          .catchAll(_ => UIO(coinAddress -> PriceQuote(0f, now))) //ignore failures.
//      )
//      balance <- ZIO
//        .mergeAllParN(5)(priceQuoteEffects)(BigDecimal(0)) { (acc, currencyQuote) =>
//          acc + positionsByCoinAddress(currencyQuote._1).map(_.numberOfCoins * currencyQuote._2.price).sum
//        }
//        .flatMap { amount =>
//          walletRepo.getQuote(wallet.address, Currency.unsafeFrom("BNB"))
//            .zipWithPar(walletRepo.getQuote(wallet.address, Currency.unsafeFrom("USDT")))((f1, f2) => f1.add(f2.amount))
//            .map(mainQuotes => FungibleData(amount, USD).add(mainQuotes.amount))
//        }
//        .orElseFail(AccountBalanceComputeError("Cannot compute account balance"))
    } yield trend.last.getOrElse(WBNB, FungibleData(0, WBNB))

  override def trend(wallet: Wallet): IO[PortfolioError, List[FungibleData]] = {
    for {
      _           <- logger.info(s"Computing account balance trend for $wallet")
      marketPlays <- marketPlaysService.getPlays(wallet).orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
    } yield List.empty
  }

  private def currencyTrend(marketPlays: MarketPlays): List[Map[Currency, FungibleData]] = {
    marketPlays.interval match {
      case Some(timeInterval) =>
        timeInterval.days().map { day =>
          val interval = TimeInterval(timeInterval.start.atBeginningOfDay(), day.atEndOfDay())
          val plays = marketPlays.plays.filter(_.inInterval(interval))
          val currencyBalance: mutable.Map[Currency, BigDecimal] = mutable.Map(WBNB -> BigDecimal(0))
          println(s"Processing ${plays.size} plays for interval $interval")
          plays.foreach {
            case Position(entries, _, _, _) => entries.foreach {
              case a: AirDrop =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - a.fee.amount)
                currencyBalance.update(a.received.currency, currencyBalance.getOrElse(a.received.currency, BigDecimal(0) + a.received.amount))
              case a: Approval =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - a.fee.amount)
              case buy: Buy =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - buy.fee.amount)
                if(buy.spentOriginal.isDefined) {
                  currencyBalance.update(buy.spentOriginal.get.currency, currencyBalance.getOrElse(buy.spentOriginal.get.currency, BigDecimal(0)) - buy.spentOriginal.get.amount)
                } else {
                  currencyBalance.update(buy.spent.currency, currencyBalance.getOrElse(buy.spent.currency, BigDecimal(0)) - buy.spent.amount)
                }
                currencyBalance.update(buy.received.currency, currencyBalance.getOrElse(buy.received.currency, BigDecimal(0)) + buy.received.amount)
              case c: Claim =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - c.fee.amount)
                currencyBalance.update(c.received.currency, currencyBalance.getOrElse(c.received.currency, BigDecimal(0)) + c.received.amount)
              case c: Contribute =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - c.fee.amount)
                currencyBalance.update(c.spent.currency, currencyBalance.getOrElse(c.spent.currency, BigDecimal(0)) - c.spent.amount)
              case s: Sell =>
                currencyBalance.update(
                  s.received.currency,
                  currencyBalance.getOrElse(s.received.currency, BigDecimal(0)) + s.received.amount
                )
                currencyBalance.update(WBNB, currencyBalance(WBNB) - s.fee.amount)
              case tIn: TransferIn =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - tIn.fee.amount)
                currencyBalance.update(tIn.value.currency, currencyBalance.getOrElse(tIn.value.currency, BigDecimal(0)) + tIn.value.amount)
              case tOut: TransferOut =>
                currencyBalance.update(WBNB, currencyBalance(WBNB) - tOut.fee.amount)
                currencyBalance.update(tOut.amount.currency, currencyBalance.getOrElse(tOut.amount.currency, BigDecimal(0)) + tOut.amount.amount)
              case _ =>
            }
            case TopUp(_, value, fee, _, _, _) =>
              currencyBalance.update(WBNB, currencyBalance(WBNB) + value.amount - fee.amount)
            case Withdraw(_, value, fee, _, _, _) =>
              currencyBalance.update(WBNB, currencyBalance(WBNB) - value.amount - fee.amount)
          }
          currencyBalance.map { case (currency, amount) => currency -> FungibleData(amount, currency) }.toMap
        }
      case None => List.empty
    }
  }
}

object LiveAccountBalance {
  lazy val layer
    : URLayer[Has[MarketPlayService] with Has[PriceQuoteRepo] with Has[WalletRepo] with Clock with Logging, Has[
      AccountBalance
    ]] =
    (LiveAccountBalance(_, _, _, _, _)).toLayer
}
