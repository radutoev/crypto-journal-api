package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ ContextId, FungibleDataTimePoint, UserId, WalletAddressPredicate }
import domain.portfolio.model.{ BinData => CJBinData, PlaysGrouping, DailyTradeData => CJDailyTradeData }
import domain.portfolio.performance.{ Performance => CJPerformance }
import domain.portfolio.{ PlaysOverview, StatsService, PlaysDistinctValues => CJPlaysDistinctValues }
import infrastructure.api.common.dto.{ FungibleData, _ }
import infrastructure.api.common.{ CountQParamOps, IntervalQParamsOps }
import infrastructure.auth.JwtRequestContext
import vo.filter.{ Count, KpiFilter }
import vo.{ TimeInterval, PeriodDistribution => CJPeriodDistribution }

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.prelude.Validation
import zio.{ Has, ZIO }

import java.time.Duration

object portfolio {
  def routes(userId: UserId, contextId: ContextId) = HttpApp.collectM {
    //dashboard endpoint
    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "kpi" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPlaysOverview(address)(kpiFilter)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       playsOverview => Response.jsonString(PortfolioKpi(playsOverview).toJson)
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "stats" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPlaysOverview(address)(kpiFilter)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi =>
                         Response.jsonString(
                           kpiFilter.count
                             .fold(PortfolioStats(portfolioKpi))(count => PortfolioStats(portfolioKpi, count))
                             .toJson
                         )
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "plays-distribution" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))
        timeFilter <- req.url.intervalFilter().toZIO.mapError(reason => BadRequest(reason))
        grouping   <- req.url.playsGrouping().toZIO.mapError(reason => BadRequest(reason))
        response <- CryptoJournalApi
                     .aggregatePlays(address, timeFilter, grouping)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       data =>
                         if (data.nonEmpty) {
                           Response.jsonString(
                             data.map { case (binName, binData) => binName.value -> BinData(binData) }.toJson
                           )
                         } else {
                           Response.status(Status.NO_CONTENT)
                         }
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "calendar" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        timeFilter <- req.url.intervalFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getMonthlyNetReturnDistribution(address, timeFilter)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       distribution =>
                         Response.jsonString(distribution.map {
                           case (day, data) => day.value -> DailyTradeData(data)
                         }.toJson)
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "stats" / "trade-summary" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPlaysOverview(address)(kpiFilter)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       playsOverview =>
                         Response.jsonString(
                           kpiFilter.count
                             .fold(TradeSummary(playsOverview))(count => TradeSummary(playsOverview, count))
                             .toJson
                         )
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "stats" / "tag-distribution" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPlaysOverview(address)(kpiFilter)
                     .provideSomeLayer[Has[StatsService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       playsOverview => Response.jsonString(TagDistribution(playsOverview.distinctValues).toJson)
                     )
      } yield response
  }

  implicit class KpiQParamsOps(url: URL) {
    def kpiFilter(): Validation[String, KpiFilter] =
      Validation.validateWith(
        Validation.succeed(url.countFilter().fold[Option[Count]](_ => None, Some(_))),
        Validation.succeed(url.intervalFilter().fold[Option[TimeInterval]](_ => None, Some(_)))
      ) {
        case (maybeCount, maybeInterval) =>
          KpiFilter(maybeCount, maybeInterval)
      }

    def playsGrouping(): Validation[String, PlaysGrouping] = {
      val rawGrouping = url.queryParams
        .get("grouping")
        .flatMap(_.headOption)
        .getOrElse("hour") //default to hour,
      Validation.fromEither(PlaysGrouping.fromString(rawGrouping))
    }
  }

  final case class PortfolioKpi(
    accountBalance: ValueTrendComparison,
    tradeCount: Int,
    winRate: Float,
    loseRate: Float,
    netReturn: ValueTrendComparison,
    avgDailyTradeCount: Float
  )

  final case class PortfolioStats(
    distinctValues: KpiDistinctValues,
    tradeSummary: TradeSummary,
    periodDistribution: PeriodDistribution,
    tagDistribution: TagDistribution
  )

  implicit val portfolioCodec: JsonCodec[PortfolioKpi]        = DeriveJsonCodec.gen[PortfolioKpi]
  implicit val portfolioStatsCodec: JsonCodec[PortfolioStats] = DeriveJsonCodec.gen[PortfolioStats]

  object PortfolioKpi {
    def apply(playsOverview: PlaysOverview): PortfolioKpi =
      new PortfolioKpi(
        ValueTrendComparison(
          playsOverview.balanceTrend.latestValue.fungibleData.asJson,
          playsOverview.balanceTrend.items.map(_.fungibleData.amount),
          Performance(playsOverview.balancePerformance)
        ),
        playsOverview.distinctValues.tradeCount,
        playsOverview.distinctValues.winRate,
        playsOverview.distinctValues.loseRate,
        ValueTrendComparison(
          playsOverview.netReturnTrend.latestValue.fungibleData.asJson,
          playsOverview.netReturnTrend.items.map(_.fungibleData.amount),
          Performance(playsOverview.netReturnPerformance)
        ),
        playsOverview.distinctValues.avgDailyTradeCount
      )
  }

  object PortfolioStats {

    def apply(playsOverview: PlaysOverview, count: Count): PortfolioStats =
      new PortfolioStats(
        distinctValues = KpiDistinctValues(playsOverview.distinctValues, playsOverview.netReturnTrend.latestValue),
        TradeSummary(playsOverview, count),
        PeriodDistribution(playsOverview.distinctValues.periodReturn()),
        TagDistribution(playsOverview.distinctValues)
      )

    def apply(playsOverview: PlaysOverview): PortfolioStats =
      new PortfolioStats(
        distinctValues = KpiDistinctValues(playsOverview.distinctValues, playsOverview.netReturnTrend.latestValue),
        TradeSummary(playsOverview),
        PeriodDistribution(playsOverview.distinctValues.periodReturn()),
        TagDistribution(playsOverview.distinctValues)
      )
  }

  final case class ValueTrendComparison(value: FungibleData, trend: List[BigDecimal], performance: Performance)

  object ValueTrendComparison {
    implicit val valueTrendComparison: JsonCodec[ValueTrendComparison] = DeriveJsonCodec.gen[ValueTrendComparison]
  }

  final case class KpiDistinctValues(
    netReturn: BigDecimal,
    biggestWin: Option[FungibleData],
    biggestLoss: Option[FungibleData],
    winRate: Float,
    loseRate: Float,
    tradeCount: Int,
    openTradesCount: Int,
    avgDailyTradeCount: Float,
    totalWins: Int,
    totalLoses: Int,
    maxConsecutiveWins: Int,
    maxConsecutiveLoses: Int,
    totalTradedCoins: BigDecimal,
    numberOfCoins: BigDecimal,
    avgWinnerHoldTime: String,
    avgLoserHoldTime: String,
    totalFees: Map[String, FungibleData]
  )

  object KpiDistinctValues {
    implicit val kpiDistinctCodec: JsonCodec[KpiDistinctValues] = DeriveJsonCodec.gen[KpiDistinctValues]

    def asHumanReadableForm(d: Duration): String =
      d.toString.substring(2).replaceAll("(\\d[HMS])(?!$)", "$1 ").toLowerCase()

    def apply(distinctValues: CJPlaysDistinctValues, latestNetReturn: FungibleDataTimePoint): KpiDistinctValues =
      new KpiDistinctValues(
        latestNetReturn.fungibleData.amount,
        distinctValues.biggestWin.map(_.asJson),
        distinctValues.biggestLoss.map(_.asJson),
        distinctValues.winRate,
        distinctValues.loseRate,
        distinctValues.tradeCount,
        distinctValues.openTradesCount,
        distinctValues.avgDailyTradeCount,
        distinctValues.totalWins.value,
        distinctValues.totalLoses.value,
        distinctValues.maxConsecutiveWins.value,
        distinctValues.maxConsecutiveLoses.value,
        distinctValues.totalCoins,
        distinctValues.numberOfCoins,
        asHumanReadableForm(distinctValues.avgWinningHoldTime),
        asHumanReadableForm(distinctValues.avgLosingHoldTime),
        distinctValues.totalFees.map { case (currency, data) => currency.value -> data.asJson }
      )
  }

  final case class TradeSummary(wins: List[CoinToFungiblePair], loses: List[CoinToFungiblePair])

  object TradeSummary {
    implicit val tradeSummaryCodec: JsonCodec[TradeSummary] = DeriveJsonCodec.gen[TradeSummary]

    def apply(playsOverview: PlaysOverview, count: Count): TradeSummary =
      new TradeSummary(
        wins = playsOverview.distinctValues.coinWins(count).map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3)),
        loses =
          playsOverview.distinctValues.coinLoses(count).map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3))
      )

    def apply(playsOverview: PlaysOverview): TradeSummary =
      new TradeSummary(
        wins = playsOverview.distinctValues.coinWins.map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3)),
        loses = playsOverview.distinctValues.coinLoses.map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3))
      )
  }

  final case class TagDistribution(
    mistakes: Map[String, FungibleDataAndPercentage],
    tags: Map[String, FungibleDataAndPercentage]
  )

  object TagDistribution {
    implicit val tagDistributionCodec: JsonCodec[TagDistribution] = DeriveJsonCodec.gen[TagDistribution]

    def apply(portfolioKpi: CJPlaysDistinctValues): TagDistribution =
      new TagDistribution(
        mistakes = portfolioKpi.mistakeContribution.map {
          case (mistake, (fungibleData, percentage)) =>
            mistake.value -> FungibleDataAndPercentage(
              FungibleData(fungibleData),
              percentage
            )
        },
        tags = portfolioKpi.tagContribution.map {
          case (setup, (fungibleData, percentage)) =>
            setup.value -> FungibleDataAndPercentage(
              FungibleData(fungibleData),
              percentage
            )
        }
      )
  }

  final case class CoinToFungiblePair(currency: String, fungibleData: FungibleData, percentage: BigDecimal)

  object CoinToFungiblePair {
    implicit val xCodec: JsonCodec[CoinToFungiblePair] = DeriveJsonCodec.gen[CoinToFungiblePair]
  }

  final case class FungibleDataAndPercentage(fungibleData: FungibleData, percentage: BigDecimal)

  object FungibleDataAndPercentage {
    implicit val fungibleDataAndPercentageCodec: JsonCodec[FungibleDataAndPercentage] =
      DeriveJsonCodec.gen[FungibleDataAndPercentage]
  }

  final case class Performance(absolute: BigDecimal, percentage: BigDecimal, trend: String)

  object Performance {
    implicit val performanceCodec: JsonCodec[Performance] = DeriveJsonCodec.gen[Performance]

    def apply(p: CJPerformance): Performance =
      new Performance(p.absolute, p.percentage, p.trend.toString)
  }

  final case class PeriodDistribution(
    weekly: List[FungibleData],
    monthly: List[FungibleData],
    yearly: Map[Int, FungibleData]
  )

  object PeriodDistribution {
    implicit val periodDistributionCodec: JsonCodec[PeriodDistribution] = DeriveJsonCodec.gen[PeriodDistribution]

    def apply(distribution: CJPeriodDistribution): PeriodDistribution =
      new PeriodDistribution(
        distribution.weekly.map(_.asJson),
        distribution.monthly.map(_.asJson),
        distribution.yearly.view.mapValues(_.asJson).toMap
      )
  }

  final case class DailyTradeData(netReturn: BigDecimal, tradeCount: Int)

  object DailyTradeData {
    implicit val dailyTradeDataCodec: JsonCodec[DailyTradeData] = DeriveJsonCodec.gen[DailyTradeData]

    def apply(data: CJDailyTradeData): DailyTradeData =
      new DailyTradeData(data.netReturn, data.tradeCount.value)
  }

  final case class BinData(
    tradeCount: Long,
    numberOfCoins: BigDecimal,
    winRate: BigDecimal,
    netReturn: FungibleData,
    returnPercentage: BigDecimal,
    fees: FungibleData
  )

  object BinData {
    implicit val binDataCodec: JsonCodec[BinData] = DeriveJsonCodec.gen[BinData]

    def apply(data: CJBinData): BinData =
      new BinData(
        tradeCount = data.tradeCount.value,
        numberOfCoins = data.numberOfCoins,
        winRate = data.winRate,
        netReturn = FungibleData(data.netReturn),
        returnPercentage = data.returnPercentage,
        fees = FungibleData(data.fees)
      )
  }
}
