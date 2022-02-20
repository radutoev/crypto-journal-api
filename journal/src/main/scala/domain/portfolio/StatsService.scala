package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.date.{ DayUnit, MinuteUnit }
import domain.model.{ BUSD, Currency, FungibleData, TradeCountPredicate, WBNB }
import domain.portfolio.PlaysDistinctValues.DayFormatter
import domain.portfolio.error.{
  InvalidPortfolioError,
  PortfolioKpiGenerationError,
  StatsError,
  TradeSummaryGenerationError
}
import domain.portfolio.model._
import domain.position._
import domain.position.error.MarketPlayError
import domain.pricequote.{ CurrencyPair, PriceQuoteService, PriceQuotes }
import domain.wallet.Wallet
import util.{ BeginningOfCrypto, InstantOps }
import vo.TimeInterval
import vo.filter.{ KpiFilter, PlayFilter }

import eu.timepit.refined.refineV
import io.softwarechain.cryptojournal.domain.portfolio.LiveStatsService.TradeSummaryOps
import zio.clock.Clock
import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer }

trait StatsService {
  def playsOverview(userWallet: Wallet, kpiFilter: KpiFilter): IO[StatsError, PlaysOverview]

  def tradesSummary(wallet: Wallet, kpiFilter: KpiFilter): IO[StatsError, TradeSummary]

  def netReturnDistributionByDay(wallet: Wallet, interval: TimeInterval): IO[StatsError, NetReturnDistributionByDay]

  def playsDistribution(
    wallet: Wallet,
    interval: TimeInterval,
    grouping: PlaysGrouping,
    withSourceData: Boolean = false
  ): IO[StatsError, Map[BinName, BinDataAndSources]]
}

final case class LiveStatsService(
  marketPlaysService: MarketPlayService,
  priceQuoteService: PriceQuoteService,
  clock: Clock.Service,
  logger: Logger[String]
) extends StatsService {
  override def playsOverview(wallet: Wallet, kpiFilter: KpiFilter): IO[StatsError, PlaysOverview] =
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
      playsWithQuotes = addQuotesToPlays(plays, quotes)
      referencePlays  <- fetchReferencePlays(wallet, filter, timeIntervalForComparison)
      referenceQuotes <- fetchQuotes(referencePlays, timeIntervalForComparison)
      //TODO I noticed that the net return values are identical with MinuteQuotes and DailyQuotes. Test more and see if I can find some differences.
//      refNetReturnQuotes        <- fetchMinuteQuotes(referencePlays, timeIntervalForComparison)
      distinctValues = new PlaysDistinctValues(playsWithQuotes, requestedInterval)
      balanceTrend   = plays.balanceTrend(requestedInterval, BUSD, quotes)
      referenceInterval = TimeInterval(
        timeIntervalForComparison.end.minusDays(refineV.unsafeFrom(requestedInterval.dayCount.value)),
        timeIntervalForComparison.end
      )
      referenceBalanceTrend   = referencePlays.balanceTrend(referenceInterval, BUSD, referenceQuotes)
      netReturnTrend          = plays.netReturn(requestedInterval, BUSD, quotes)
      referenceNetReturnTrend = referencePlays.netReturn(referenceInterval, BUSD, referenceQuotes)
    } yield PlaysOverview(
      distinctValues,
      balanceTrend,
      balanceTrend.performance(referenceBalanceTrend),
      netReturnTrend,
      netReturnTrend.performance(referenceNetReturnTrend)
    )

  override def tradesSummary(wallet: Wallet, kpiFilter: KpiFilter): IO[StatsError, TradeSummary] = {
    val interval = kpiFilter.interval.get //TODO Should I default or fail?? I think I need to change KpiFilter honestly.
    val summaryEffect = for {
      positions <- marketPlaysService
                    .playStream(wallet, interval)
                    .filter(play => play.isInstanceOf[Position])
                    .map(play => play.asInstanceOf[Position])
                    .filter(_.isClosed)
                    .runCollect
                    .map(
                      _.toList.reverse
                    ) //reversing here because at the moment I have defined the index like so:   MarketPlayTest	address ASC  openedAt DESC; I don't know if its worth adding another one.
      //TODO Doing these results in fetching a lot of quotes by key (10k order) if the time interval is large.
      quotes         <- fetchQuotes(MarketPlays(positions), interval)
      marketPlays    = MarketPlays(positions.map(p => p.copy(dataSource = Some(PriceQuotePositionData(quotes)))))
      distinctValues = PlaysDistinctValues(marketPlays)
    } yield TradeSummary(
      wins = kpiFilter.count
        .map(count => distinctValues.coinWins(count))
        .getOrElse(distinctValues.coinWins)
        .map(t => CoinToFungiblePair.apply(t._1, t._2, t._3)),
      loses = kpiFilter.count
        .map(count => distinctValues.coinLoses(count))
        .getOrElse(distinctValues.coinLoses)
        .map(t => CoinToFungiblePair.apply(t._1, t._2, t._3))
    )

    summaryEffect.orElseFail(TradeSummaryGenerationError("Unable to generate trades summary"))
  }

  override def netReturnDistributionByDay(
    wallet: Wallet,
    interval: TimeInterval
  ): IO[StatsError, NetReturnDistributionByDay] =
    for {
      filter <- PlayFilter(Int.MaxValue, interval).toZIO.mapError(InvalidPortfolioError)
      plays  <- marketPlaysService.getPlays(wallet, filter, withQuotes = false).mapError(statsErrorMapper)
      quotes <- fetchQuotes(plays, interval)
    } yield dailyContribution(plays, interval, quotes, BUSD)

  override def playsDistribution(
    wallet: Wallet,
    interval: TimeInterval,
    grouping: PlaysGrouping,
    withSourceData: Boolean = false
  ): IO[StatsError, Map[BinName, BinDataAndSources]] =
    for {
      filter <- PlayFilter(Int.MaxValue, interval).toZIO.mapError(InvalidPortfolioError)
      plays  <- marketPlaysService.getPlays(wallet, filter, withQuotes = false).mapError(statsErrorMapper)
      quotes <- fetchQuotes(plays, interval) //TODO this is daily, does it make sense to get by minute??
    } yield groupPlays(plays, quotes, grouping, withSourceData)

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

  private def fetchMinuteQuotes(plays: MarketPlays, interval: TimeInterval): IO[StatsError, PriceQuotes] = {
    val fetchQuotesEffect = for {
      bnbUsdQuotes   <- priceQuoteService.getQuotes(CurrencyPair(WBNB, BUSD), interval, MinuteUnit)
      coinsBnbQuotes <- priceQuoteService.getQuotes(plays.currencies.map(_._1), WBNB, interval, MinuteUnit)
    } yield bnbUsdQuotes.merge(coinsBnbQuotes)
    fetchQuotesEffect.orElseFail(PortfolioKpiGenerationError("Unable to fetch quotes"))
  }

  /**
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

  private[portfolio] def dailyContribution(
    plays: MarketPlays,
    interval: TimeInterval,
    quotes: PriceQuotes,
    targetCurrency: Currency
  ): NetReturnDistributionByDay = {
    val netReturnValues = plays.closedPositions
      .map(p => p.copy(dataSource = Some(PriceQuotePositionData(quotes)))) //TODO I need this because closedAt is used in the dataSource trait, so I have to specify the quotes. Find a way to do this better.
      .map(p => p.closedAt.get.atBeginningOfDay() -> p)
      .groupBy(_._1)
      .map {
        case (day, list) =>
          val dailyPositions = list.map(_._2)
          val dailyTradeData = DailyTradeData(
            MarketPlays(dailyPositions)
              .netReturn(TimeInterval(day.atBeginningOfDay(), day.atEndOfDay()), targetCurrency, quotes)
              .latestValue
              .fungibleData
              .amount,
            refineV[TradeCountPredicate].unsafeFrom(dailyPositions.size)
          )
          refineV[DayPredicate].unsafeFrom(DayFormatter.format(day)) -> dailyTradeData
      }

    interval
      .days()
      .map { day =>
        val dayFormat = refineV[DayPredicate].unsafeFrom(DayFormatter.format(day))
        dayFormat -> netReturnValues.getOrElse(
          dayFormat,
          DailyTradeData(BigDecimal(0), refineV[TradeCountPredicate].unsafeFrom(0))
        )
      }
      .toMap
  }

  private[portfolio] def groupPlays(
    marketPlays: MarketPlays,
    quotes: PriceQuotes,
    grouping: PlaysGrouping,
    withSourceData: Boolean
  ): Map[BinName, BinDataAndSources] =
    marketPlays.closedPositions
    //TODO I need this because closedAt is used in the dataSource trait, so I have to specify the quotes. Find a way to do this better.
      .map(p => p.copy(dataSource = Some(PriceQuotePositionData(quotes))))
      .map(p => grouping.bin(p) -> p)
      .collect {
        case (Some(names), position) => names.map(binName => binName -> position)
      }
      .flatten
      .groupBy(_._1)
      .view
      .mapValues(_.map(_._2))
      .mapValues(items =>
        BinDataAndSources(BinData(MarketPlays(items), quotes), if (withSourceData) items else Nil, withSourceData)
      )
      .toMap

  private def statsErrorMapper(positionError: MarketPlayError): StatsError =
    PortfolioKpiGenerationError("Unable to generate KPIs")
}

object LiveStatsService {
  lazy val layer
    : URLayer[Has[MarketPlayService] with Has[PriceQuoteService] with Clock with Logging, Has[StatsService]] =
    (LiveStatsService(_, _, _, _)).toLayer

  implicit class TradeSummaryOps(tradeSummary: TradeSummary) {
    def addPositions(plays: MarketPlays, maxCount: Int): TradeSummary = {
      val distinctValues = PlaysDistinctValues(plays)

      tradeSummary.copy(
        wins =
          (tradeSummary.wins ++ distinctValues.coinWins.map(t => CoinToFungiblePair(t._1, t._2, t._3))).take(maxCount),
        loses =
          tradeSummary.loses ++ distinctValues.coinLoses.map(t => CoinToFungiblePair(t._1, t._2, t._3)).take(maxCount)
      )
    }
  }
}
