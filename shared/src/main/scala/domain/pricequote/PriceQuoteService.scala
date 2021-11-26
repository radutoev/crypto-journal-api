package io.softwarechain.cryptojournal
package domain.pricequote

import domain.blockchain.BlockchainRepo
import domain.model.{ CoinAddress, Currency }
import domain.pricequote.LivePriceQuoteService.BeginningOfTime
import domain.pricequote.error.PriceQuoteError
import util.InstantOps
import vo.TimeInterval

import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, URLayer, ZIO }

import java.time.Instant

trait PriceQuoteService {
  def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, PriceQuotes]

  def updateQuotes(currencies: Set[(Currency, CoinAddress)]): IO[PriceQuoteError, Unit]
}

final case class LivePriceQuoteService(
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: BlockchainRepo,
  clock: Clock.Service,
  logger: Logger[String]
) extends PriceQuoteService {
  override def getQuotes(
    currencies: Set[Currency],
    timeInterval: TimeInterval
  ): IO[PriceQuoteError, PriceQuotes] =
    priceQuoteRepo
      .getQuotes(Set.empty, timeInterval)
      .map(PriceQuotes(_))

  override def updateQuotes(currencies: Set[(Currency, CoinAddress)]): IO[PriceQuoteError, Unit] =
    for {
      _                 <- logger.info(s"Update price quotes for currencies: ${currencies.mkString(",")}")
      latestQuotes      <- priceQuoteRepo.getLatestQuotes(currencies.map(_._1))
      today             <- clock.instant.map(_.atBeginningOfDay())
      addressToCurrency = currencies.toMap.map(_.swap)
      intervals = currencies.map { currency =>
        if (latestQuotes.contains(currency._1)) {
          currency._2 -> TimeInterval(latestQuotes(currency._1).timestamp, today)
        } else {
          currency._2 -> TimeInterval(BeginningOfTime, today)
        }
      }
      _ <- ZIO.foreachParN_(4)(intervals) {
            case (coinAddress, interval) =>
              blockchainRepo
                .getHistoricalPriceQuotes(coinAddress, interval)
                .flatMap(quotes => priceQuoteRepo.saveQuotes(Map(addressToCurrency(coinAddress) -> quotes)))
                .ignore
          }
    } yield ()
}

object LivePriceQuoteService {
  lazy val layer
    : URLayer[Has[PriceQuoteRepo] with Has[BlockchainRepo] with Clock with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _, _)).toLayer

  private[pricequote] val BeginningOfTime = Instant.parse("2016-01-01T00:00:00.000Z")
}
