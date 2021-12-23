package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model.{ ContextId, UserId, WalletAddressPredicate }
import domain.portfolio.{ AccountBalance, KpiService, NetReturn, PortfolioKpi => CJPortfolioKpi }
import domain.portfolio.model.{ DailyTradeData => CJDailyTradeData, Performance => CJPerformance }
import infrastructure.api.common.{ CountQParamOps, IntervalQParamsOps }
import infrastructure.api.common.dto.FungibleData
import infrastructure.api.common.dto._
import infrastructure.auth.JwtRequestContext
import vo.{ TimeInterval, PeriodDistribution => CJPeriodDistribution }
import vo.filter.{ Count, KpiFilter }
import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.prelude.Validation
import zio.{ Has, ZIO }

import java.time.Duration

object portfolio {
  def routes(userId: UserId, contextId: ContextId) = HttpApp.collectM {
    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "kpi" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi => Response.jsonString(PortfolioKpi(portfolioKpi).toJson)
                     )
      } yield response

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "stats" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtRequestContext.layer(userId, contextId))
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

    case req @ Method.GET -> Root / "portfolio" / rawWalletAddress / "daily-distribution" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        kpiFilter <- req.url.kpiFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi =>
                         Response.jsonString(portfolioKpi.dailyContribution.map {
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
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi =>
                         Response.jsonString(
                           kpiFilter.count
                             .fold(TradeSummary(portfolioKpi))(count => TradeSummary(portfolioKpi, count))
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
                     .getPortfolioKpis(address)(kpiFilter)
                     .provideSomeLayer[Has[KpiService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       _ => Response.status(Status.INTERNAL_SERVER_ERROR),
                       portfolioKpi => Response.jsonString(TagDistribution(portfolioKpi).toJson)
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
    def apply(kpi: CJPortfolioKpi): PortfolioKpi =
      new PortfolioKpi(
        ValueTrendComparison.fromAccountBalance(kpi.accountBalance, AccountBalance(kpi.referenceMarketPlays)),
        kpi.tradeCount,
        kpi.winRate,
        kpi.loseRate,
        ValueTrendComparison.fromNetReturn(kpi.netReturn, NetReturn(kpi.referenceMarketPlays)),
        kpi.avgDailyTradeCount
      )
  }

  object PortfolioStats {

    def apply(portfolioKpi: CJPortfolioKpi, count: Count): PortfolioStats =
      new PortfolioStats(
        distinctValues = KpiDistinctValues(portfolioKpi),
        TradeSummary(portfolioKpi, count),
        PeriodDistribution(portfolioKpi.periodReturn()),
        TagDistribution(portfolioKpi)
      )

    def apply(portfolioKpi: CJPortfolioKpi): PortfolioStats =
      new PortfolioStats(
        distinctValues = KpiDistinctValues(portfolioKpi),
        TradeSummary(portfolioKpi),
        PeriodDistribution(portfolioKpi.periodReturn()),
        TagDistribution(portfolioKpi)
      )
  }

  final case class ValueTrendComparison(value: FungibleData, trend: List[BigDecimal], performance: Performance)

  object ValueTrendComparison {
    implicit val valueTrendComparison: JsonCodec[ValueTrendComparison] = DeriveJsonCodec.gen[ValueTrendComparison]

    def fromNetReturn(netReturn: NetReturn, compareWith: NetReturn): ValueTrendComparison =
      new ValueTrendComparison(
        netReturn.value.asJson,
        netReturn.trend.map(_.amount),
        Performance(netReturn.performance(compareWith))
      )

    def fromAccountBalance(balance: AccountBalance, compareWith: AccountBalance): ValueTrendComparison =
      new ValueTrendComparison(
        balance.value.asJson,
        balance.trend.map(_.amount),
        Performance(balance.performance(compareWith))
      )
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

    def apply(portfolio: CJPortfolioKpi): KpiDistinctValues =
      new KpiDistinctValues(
        portfolio.netReturn.value.amount,
        portfolio.biggestWin.map(_.asJson),
        portfolio.biggestLoss.map(_.asJson),
        portfolio.winRate,
        portfolio.loseRate,
        portfolio.tradeCount,
        portfolio.openTradesCount,
        portfolio.avgDailyTradeCount,
        portfolio.totalWins.value,
        portfolio.totalLoses.value,
        portfolio.maxConsecutiveWins.value,
        portfolio.maxConsecutiveLoses.value,
        portfolio.totalCoins,
        portfolio.numberOfCoins,
        asHumanReadableForm(portfolio.avgWinningHoldTime),
        asHumanReadableForm(portfolio.avgLosingHoldTime),
        portfolio.totalFees.map { case (currency, data) => currency.value -> data.asJson }
      )
  }

  final case class TradeSummary(wins: List[CoinToFungiblePair], loses: List[CoinToFungiblePair])

  object TradeSummary {
    implicit val tradeSummaryCodec: JsonCodec[TradeSummary] = DeriveJsonCodec.gen[TradeSummary]

    def apply(portfolio: CJPortfolioKpi, count: Count): TradeSummary =
      new TradeSummary(
        wins = portfolio.coinWins(count).map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3)),
        loses = portfolio.coinLoses(count).map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3))
      )

    def apply(portfolio: CJPortfolioKpi): TradeSummary =
      new TradeSummary(
        wins = portfolio.coinWins.map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3)),
        loses = portfolio.coinLoses.map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3))
      )
  }

  final case class TagDistribution(
    mistakes: Map[String, FungibleDataAndPercentage],
    tags: Map[String, FungibleDataAndPercentage]
  )

  object TagDistribution {
    implicit val tagDistributionCodec: JsonCodec[TagDistribution] = DeriveJsonCodec.gen[TagDistribution]

    def apply(portfolioKpi: CJPortfolioKpi): TagDistribution =
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
      new DailyTradeData(data.netReturn.value.amount, data.tradeCount.value)
  }
}
