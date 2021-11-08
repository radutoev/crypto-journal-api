package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{FungibleData, USD}
import domain.portfolio.error.{AccountBalanceComputeError, PortfolioError}
import domain.position.{MarketPlayService, Position}
import domain.pricequote.error.PriceQuoteNotFound
import domain.pricequote.{PriceQuote, PriceQuoteRepo}
import domain.wallet.Wallet

import zio.clock.Clock
import zio.logging.{Logger, Logging}
import zio.{Has, IO, UIO, URLayer, ZIO}

trait AccountBalance {
  def value(wallet: Wallet): IO[PortfolioError, FungibleData]
}

final case class LiveAccountBalance(
  marketPlaysService: MarketPlayService,
  priceQuoteRepo: PriceQuoteRepo,
  clock: Clock.Service,
  logger: Logger[String]
) extends AccountBalance {
  //TODO How can I find the BNB value in the wallet?
  override def value(wallet: Wallet): IO[PortfolioError, FungibleData] =
    for {
      _ <- logger.info(s"Computing account balance for $wallet")
      now <- clock.instant
      marketPlays <- marketPlaysService
                      .getPlays(wallet)
                      .orElseFail(AccountBalanceComputeError("MarketPlays fetch error"))
      positionsByCoinAddress = marketPlays.plays.collect {
        case p: Position if p.coinAddress.isDefined => p.coinAddress.get -> p
      }.groupBy(_._1).view.mapValues(_.map(_._2)).toMap
      priceQuoteEffects = positionsByCoinAddress.keySet.map(coinAddress =>
        priceQuoteRepo.getCurrentQuote(coinAddress)
          .map(quote => coinAddress -> quote)
          .catchSome {
            case PriceQuoteNotFound(contract) => UIO(contract -> PriceQuote(0f, now))
          }
      )
      balance <- ZIO
                  .mergeAllParN(2)(priceQuoteEffects)(BigDecimal(0)) { (acc, currencyQuote) =>
                    acc + positionsByCoinAddress(currencyQuote._1).map(_.numberOfCoins * currencyQuote._2.price).sum
                  }
                  .orElseFail(AccountBalanceComputeError("Cannot compute account balance"))
    } yield FungibleData(balance, USD)
}

object LiveAccountBalance {
  lazy val layer: URLayer[Has[MarketPlayService] with Has[PriceQuoteRepo] with Clock with Logging, Has[AccountBalance]] =
    (LiveAccountBalance(_, _, _, _)).toLayer
}

//final case class AccountBalance1(marketPlays: MarketPlays) {
//  //TODO Should the value for the account balance take into consideration the fees as well?
//  lazy val value: FungibleData = FungibleData.zero(USD)
////    marketPlays.plays
////    .filterNot {
////      case p: Position => p.journal.exists(_.scamStrategy.exists(_ == HideFromStats))
////      case _ => false
////    }
////    .map(_.fiatValue())
////    .sumFungibleData()
//
//  //TODO Reimplement this
//  lazy val trend: List[FungibleData] = List.empty // marketPlays.trend(_.fiatValue())
//
//  def performance(relativeTo: AccountBalance): Performance = ???
////    value.compare(relativeTo.value) match {
////      case Left(_) => NoChangeInPerformance
////      case Right(comparisonResult) =>
////        comparisonResult match {
////          case Equal => NoChangeInPerformance
////          case Bigger =>
////            Performance(
////              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
////              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
////              trend = Increase
////            )
////          case Lower =>
////            Performance(
////              absolute = value.difference(relativeTo.value).getOrElse(BigDecimal(0)),
////              percentage = value.percentageDifference(relativeTo.value).getOrElse(BigDecimal(0)),
////              trend = Decrease
////            )
////        }
////    }
//}
