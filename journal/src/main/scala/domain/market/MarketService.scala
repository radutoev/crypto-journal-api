package io.softwarechain.cryptojournal
package domain.market

import domain.market.error._
import domain.model._
import domain.position.MarketPlayService
import domain.pricequote.CurrencyAddressPair
import infrastructure.bitquery.BitQueryFacade
import util.InstantOps
import vo.TimeInterval

import eu.timepit.refined.refineV
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer }

trait MarketService {
  def getHistoricalOhlcv(positionId: PlayId): IO[MarketError, List[Ohlcv]]
}

final case class LiveMarketService(
  marketPlayService: MarketPlayService,
  bitQueryFacade: BitQueryFacade,
  logger: Logger[String]
) extends MarketService {
  override def getHistoricalOhlcv(positionId: PlayId): IO[MarketError, List[Ohlcv]] =
    (for {
      position <- marketPlayService.getPosition(positionId)
      ohlvc <- if (position.coinAddress.isDefined && position.currency.isDefined) {
                bitQueryFacade.getOhlcv(
                  CurrencyAddressPair(
                    base = CurrencyAddress(position.currency.get, position.coinAddress.get),
                    quote = CurrencyAddress(WBNB, CoinAddress.unsafeFrom("0xbb4cdb9cbd36b01bd1cbaebf2de08d9173bc095c"))
                  ),
                  TimeInterval(
                    position.timeInterval.start.minusHours(refineV.unsafeFrom(1)),
                    position.timeInterval.end.plusHours(refineV.unsafeFrom(1))
                  )
                )
              } else {
                UIO(List.empty)
              }
    } yield ohlvc).orElseFail(HistoricalDataGetError("Unable to fetch ohlcv data"))
}

object LiveMarketService {
  lazy val layer: URLayer[Has[MarketPlayService] with Has[BitQueryFacade] with Logging, Has[MarketService]] =
    (LiveMarketService(_, _, _)).toLayer
}
