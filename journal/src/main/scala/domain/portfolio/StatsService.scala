package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.date.DayUnit
import domain.model.{ BUSD, WBNB }
import domain.portfolio.error.{ InvalidPortfolioError, PortfolioKpiGenerationError, StatsError }
import domain.position.error.MarketPlayError
import domain.position.{
  MarketPlayService,
  MarketPlays,
  Position,
  PriceQuotePositionData,
  PriceQuoteTopUpData,
  PriceQuoteWithdrawData,
  TopUp,
  Withdraw
}
import domain.pricequote.{ CurrencyPair, PriceQuoteService, PriceQuotes }
import domain.wallet.Wallet
import util.{ BeginningOfCrypto, InstantOps }
import vo.TimeInterval
import vo.filter.{ KpiFilter, PlayFilter }

import eu.timepit.refined.refineV
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer }

trait StatsService {
  def playsOverview(userWallet: Wallet)(kpiFilter: KpiFilter): IO[StatsError, PlaysOverview]
}

final case class LiveStatsService(
  marketPlaysService: MarketPlayService,
  priceQuoteService: PriceQuoteService,
  clock: Clock.Service,
  logger: Logger[String]
) extends StatsService {
  override def playsOverview(wallet: Wallet)(kpiFilter: KpiFilter): IO[StatsError, PlaysOverview] =
    for {
      _                         <- logger.info(s"Fetching KPIs for wallet ${wallet.address}")
      now                       <- clock.instant
      requestedInterval         = kpiFilter.interval.getOrElse(TimeInterval(BeginningOfCrypto, now))
      timeInterval              = TimeInterval(BeginningOfCrypto, requestedInterval.end)
      timeIntervalForComparison = intervalForComparePositions(requestedInterval)
      count                     = kpiFilter.count.getOrElse(30) //TODO Inspect this!
      filter                    <- PlayFilter(count, timeInterval).toZIO.mapError(InvalidPortfolioError)
      plays                     <- marketPlaysService.getPlays(wallet, filter, withQuotes = false).mapError(statsErrorMapper)
      quotes                    <- fetchQuotes(plays, timeInterval)
      //TODO I don't like that we pass the quotes as param for account balance, but copy the quotes in order to generate distinct values
      playsWithQuotes           = addQuotesToPlays(plays, quotes)
      referencePlays            <- fetchReferencePlays(wallet, filter, timeIntervalForComparison)
      referenceQuotes           <- fetchQuotes(referencePlays, timeIntervalForComparison)
      distinctValues            = new PlaysDistinctValues(playsWithQuotes, requestedInterval)
      balanceTrend              = plays.balanceTrend(requestedInterval, BUSD, quotes)
      referenceBalanceTrend     = referencePlays.balanceTrend(
        TimeInterval(timeIntervalForComparison.end.minusDays(refineV.unsafeFrom(requestedInterval.dayCount.value)), timeIntervalForComparison.end),
        BUSD,
        referenceQuotes
      )
      netReturnTrend            = plays.netReturn(requestedInterval, BUSD, quotes)
      referenceNetReturnTrend   = referencePlays.netReturn(timeIntervalForComparison, BUSD, quotes)
    } yield PlaysOverview(
      distinctValues,
      balanceTrend,
      balanceTrend.performance(referenceBalanceTrend),
      netReturnTrend,
      netReturnTrend.performance(referenceNetReturnTrend)
    )

  /**
   * Creates a new timestamp to be used for retrieving positions that will be used for performance generation.
   * For example, if somebody has a filter with an interval of 5 days, so (t1, now), we need to fetch positions
   * for the following interval: (t2, (t1 - 1)). where t1 needs to be set at the beginning of day, and t1-1 and the end of the day.
   */
  private def intervalForComparePositions(original: TimeInterval): TimeInterval = {
    val newInterval = original.minus(refineV.unsafeFrom(original.dayCount.value + 1))
    val newStart = if (newInterval.start.isAfter(BeginningOfCrypto)) {
      newInterval.start.atBeginningOfDay()
    } else BeginningOfCrypto
    newInterval.copy(start = newStart, end = newInterval.end.atEndOfDay())
  }

  private def fetchReferencePlays(
    wallet: Wallet,
    filter: PlayFilter,
    referenceInterval: TimeInterval
  ): IO[StatsError, MarketPlays] =
    if (filter.interval.start == BeginningOfCrypto) {
      marketPlaysService
        .getPlays(
          wallet,
          filter.copy(interval = referenceInterval),
          withQuotes = false
        )
        .mapError(statsErrorMapper)
    } else UIO(MarketPlays.empty())

  private def fetchQuotes(plays: MarketPlays, interval: TimeInterval): IO[StatsError, PriceQuotes] = {
    val fetchQuotesEffect = for {
      bnbUsdQuotes   <- priceQuoteService.getQuotes(CurrencyPair(WBNB, BUSD), interval, DayUnit)
      coinsBnbQuotes <- priceQuoteService.getQuotes(plays.currencies.map(_._1), WBNB, interval, DayUnit)
    } yield bnbUsdQuotes.merge(coinsBnbQuotes)
    fetchQuotesEffect.orElseFail(PortfolioKpiGenerationError("Unable to fetch quotes"))
  }

  /**
   *
   * @param marketPlays
   * @param quotes
   * @return
   */
  private def addQuotesToPlays(marketPlays: MarketPlays, quotes: PriceQuotes): MarketPlays =
    marketPlays.copy(plays = marketPlays.plays.map {
      case p: Position => p.copy(dataSource = Some(PriceQuotePositionData(quotes)))
      case t: TopUp    => t.copy(topUpDataGenerator = Some(PriceQuoteTopUpData(quotes)))
      case w: Withdraw => w.copy(withdrawDataGenerator = Some(PriceQuoteWithdrawData(quotes)))
    })

  private def statsErrorMapper(positionError: MarketPlayError): StatsError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveStatsService {
  lazy val layer
    : URLayer[Has[MarketPlayService] with Has[PriceQuoteService] with Clock with Logging, Has[StatsService]] =
    (LiveStatsService(_, _, _, _)).toLayer
}
