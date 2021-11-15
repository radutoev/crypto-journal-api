package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ Currency, FungibleData, USD }
import domain.portfolio.error.{ AccountBalanceComputeError, PortfolioError }
import domain.position.{ MarketPlayService, Position }
import domain.pricequote.error.PriceQuoteNotFound
import domain.pricequote.{ PriceQuote, PriceQuoteRepo }
import domain.wallet.{ Wallet, WalletRepo }

import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer, ZIO }

trait AccountBalance {
  def value(wallet: Wallet): IO[PortfolioError, FungibleData]
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

      priceQuoteEffects = positionsByCoinAddress.keySet.map(coinAddress =>
        priceQuoteRepo
          .getCurrentQuote(coinAddress)
          .map(quote => coinAddress -> quote)
          .catchSome {
            case PriceQuoteNotFound(contract) => UIO(contract -> PriceQuote(0f, now))
          }
      )
      balance <- ZIO
        .mergeAllParN(5)(priceQuoteEffects)(BigDecimal(0)) { (acc, currencyQuote) =>
          acc + positionsByCoinAddress(currencyQuote._1).map(_.numberOfCoins * currencyQuote._2.price).sum
        }
        .flatMap { amount =>
          walletRepo.getQuote(wallet.address, Currency.unsafeFrom("BNB"))
            .zipWithPar(walletRepo.getQuote(wallet.address, Currency.unsafeFrom("USDT")))((f1, f2) => f1.add(f2.amount))
            .map(mainQuotes => FungibleData(amount, USD).add(mainQuotes.amount))
        }
        .orElseFail(AccountBalanceComputeError("Cannot compute account balance"))
    } yield balance
}

object LiveAccountBalance {
  lazy val layer
    : URLayer[Has[MarketPlayService] with Has[PriceQuoteRepo] with Has[WalletRepo] with Clock with Logging, Has[
      AccountBalance
    ]] =
    (LiveAccountBalance(_, _, _, _, _)).toLayer
}
