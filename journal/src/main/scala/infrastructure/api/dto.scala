package io.softwarechain.cryptojournal
package infrastructure.api

import domain.market.{Ohlcv => CJOhlcv}
import domain.model.{MistakePredicate, PlayIdPredicate, TagPredicate, FungibleData => CJFungibleData}
import domain.portfolio.model.{DailyTradeData => CJDailyTradeData, Performance => CJPerformance}
import domain.portfolio.{AccountBalance, NetReturn, PortfolioKpi => CJPortfolioKpi}
import domain.position.{JournalEntry => CJJournalEntry, MarketPlay => CJMarketPlay, Position => CJPosition, PositionEntry => CJPositionEntry, PositionJournalEntry => CJPositionJournalEntry, TransferIn => CJTransferIn, TransferOut => CJTransferOut}
import domain.position.model.ScamStrategy
import domain.pricequote.{PriceQuotes, PriceQuote => CJPriceQuote}
import domain.wallet.{Wallet => CJWallet}
import vo.filter.Count
import vo.{PeriodDistribution => CJPeriodDistribution}
import infrastructure.api.dto.MarketPlay._
import util.ListOps

import eu.timepit.refined.refineV
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.{Duration, Instant}

object dto {
  sealed trait MarketPlay

  final case class TransferIn(
    hash: String,
    value: FungibleData,
    fiatValue: Option[FungibleData],
    fee: FungibleData,
    totalFees: Option[FungibleData],
    timestamp: Instant
  ) extends MarketPlay

  final case class TransferOut(
    hash: String,
    value: FungibleData,
    fiatValue: Option[FungibleData],
    fee: FungibleData,
    totalFees: Option[FungibleData],
    timestamp: Instant
  ) extends MarketPlay

  final case class Position(
    currency: String,
    state: String,
    openedAt: Instant,
    closedAt: Option[Instant],
    totalCost: Option[FungibleData],
    totalFees: Option[FungibleData],
    fiatReturn: Option[FungibleData],
    totalCoins: FungibleData,
    orderSize: FungibleData,
    averageOrderSize: FungibleData,
    entryPrice: Option[PriceQuote],
    exitPrice: Option[PriceQuote],
    numberOfExecutions: Int,
    holdTime: Option[Long],
    win: Option[Boolean],
    entries: List[PositionEntry],
    id: Option[String],
    journalEntry: JournalEntry
  ) extends MarketPlay

  final case class PositionEntry(
    `type`: String,
    value: FungibleData,
    fiatValue: Option[FungibleData],
    fee: FungibleData,
    fiatFee: Option[FungibleData],
    timestamp: Instant,
    id: Option[String]
  )

  final case class FungibleData(amount: BigDecimal, currency: String)

  final case class PriceQuote(price: Float, timestamp: Instant)

  object MarketPlay {
    implicit val priceQuoteCodec: JsonCodec[PriceQuote]       = DeriveJsonCodec.gen[PriceQuote]
    implicit val fungibleDataCodec: JsonCodec[FungibleData]   = DeriveJsonCodec.gen[FungibleData]
    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]
    implicit val transferInCodec: JsonCodec[TransferIn]       = DeriveJsonCodec.gen[TransferIn]
    implicit val marketPlayCodec: JsonCodec[MarketPlay]       = DeriveJsonCodec.gen[MarketPlay]

    def fromMarketPlay(m: CJMarketPlay): MarketPlay =
      m match {
        case pos: CJPosition => fromPosition(pos)
        case t: CJTransferIn => fromTransferIn(t)
        case tOut: CJTransferOut => fromTransferOut(tOut)
      }

    def fromPosition(position: CJPosition): Position =
      Position(
        position.currency.value,
        position.state.toString,
        position.openedAt,
        position.closedAt(),
        position.totalCost().asJson,
        position.totalFees().asJson,
        position.fiatReturn().asJson,
        position.totalCoins().asJson,
        position.orderSize().asJson,
        position.averageOrderSize().asJson,
        position.entryPrice().asJson,
        position.exitPrice().asJson,
        position.numberOfExecutions(),
        position.holdTime(),
        position.isWin().map(isWin => if (isWin) true else false),
        position.entries.map(entry => fromPositionEntry(entry)(position.priceQuotes.getOrElse(PriceQuotes.empty()))),
        position.id.map(_.value),
        position.journal.map(_.toDto).getOrElse(JournalEntry(None, List.empty, List.empty, None))
      )

    private def fromPositionEntry(entry: CJPositionEntry)(implicit priceQuotes: PriceQuotes): PositionEntry =
      PositionEntry(
        entry.`type`.toString,
        entry.value.asJson,
        entry.fiatValue.map(_.asJson),
        entry.fee.asJson,
        entry.fiatFee.map(_.asJson),
        entry.timestamp,
        entry.id.map(_.value)
      )

    private def fromTransferIn(t: CJTransferIn): TransferIn =
      TransferIn(
        hash = t.txHash.value,
        value = t.value.asJson,
        fiatValue = t.fiatValue().map(_.asJson),
        fee = t.fee.asJson,
        totalFees = t.totalFees().map(_.asJson),
        timestamp = t.timestamp
      )

    private def fromTransferOut(t: CJTransferOut): TransferOut =
      TransferOut(
        hash = t.txHash.value,
        value = t.value.asJson,
        fiatValue = t.fiatValue().map(_.asJson),
        fee = t.fee.asJson,
        totalFees = t.totalFees().map(_.asJson),
        timestamp = t.timestamp
      )
  }

  final case class Wallet(userId: String, address: String)

  object Wallet {
    implicit val walletCodec: JsonCodec[Wallet] = DeriveJsonCodec.gen[Wallet]

    def fromWallet(wallet: CJWallet): Wallet =
      Wallet(wallet.userId.value, wallet.address.value)
  }

  implicit class FungibleDataOps(data: CJFungibleData) {
    def asJson: FungibleData = FungibleData(data.amount, data.currency.value)
  }

  implicit class OptionalFungibleDataOps(data: Option[CJFungibleData]) {
    def asJson: Option[FungibleData] = data.map(_.asJson)
  }

  implicit class PriceQuoteOps(data: CJPriceQuote) {
    def asJson: PriceQuote = PriceQuote(data.price, data.timestamp)
  }

  implicit class OptionPriceQuoteOps(data: Option[CJPriceQuote]) {
    def asJson: Option[PriceQuote] = data.map(_.asJson)
  }

  final case class PortfolioKpi(
    accountBalance: ValueTrendComparison,
    tradeCount: Int,
    winRate: Float,
    loseRate: Float,
    netReturn: ValueTrendComparison,
    avgDailyTradeCount: Float
  )

  object PortfolioKpi {
    implicit val portfolioCodec: JsonCodec[PortfolioKpi] = DeriveJsonCodec.gen[PortfolioKpi]

    def apply(kpi: CJPortfolioKpi): PortfolioKpi =
      new PortfolioKpi(
        ValueTrendComparison.fromAccountBalance(kpi.balance, AccountBalance(kpi.referenceMarketPlays)),
        kpi.tradeCount,
        kpi.winRate,
        kpi.loseRate,
        ValueTrendComparison.fromNetReturn(kpi.netReturn, NetReturn(kpi.referenceMarketPlays)),
        kpi.avgDailyTradeCount
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

  final case class Performance(absolute: BigDecimal, percentage: BigDecimal, trend: String)

  object Performance {
    implicit val performanceCodec: JsonCodec[Performance] = DeriveJsonCodec.gen[Performance]

    def apply(p: CJPerformance): Performance =
      new Performance(p.absolute, p.percentage, p.trend.toString)
  }

  final case class PortfolioStats(
    distinctValues: KpiDistinctValues,
    tradeSummary: TradeSummary,
    periodDistribution: PeriodDistribution,
    tagDistribution: TagDistribution
  )

  object PortfolioStats {
    implicit val portfolioStatsCodec: JsonCodec[PortfolioStats] = DeriveJsonCodec.gen[PortfolioStats]

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
    avgWinnerHoldTime: String,
    avgLoserHoldTime: String,
    totalFees: FungibleData
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
        asHumanReadableForm(portfolio.avgWinningHoldTime),
        asHumanReadableForm(portfolio.avgLosingHoldTime),
        portfolio.totalFees.asJson
      )
  }

  final case class CoinToFungiblePair(currency: String, fungibleData: FungibleData, percentage: BigDecimal)

  object CoinToFungiblePair {
    implicit val xCodec: JsonCodec[CoinToFungiblePair] = DeriveJsonCodec.gen[CoinToFungiblePair]
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
        wins = portfolio.coinWins().map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3)),
        loses = portfolio.coinLoses().map(t => CoinToFungiblePair(t._1.value, t._2.asJson, t._3))
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
              FungibleData(fungibleData.amount, fungibleData.currency.value),
              percentage
            )
        },
        tags = portfolioKpi.tagContribution.map {
          case (setup, (fungibleData, percentage)) =>
            setup.value -> FungibleDataAndPercentage(
              FungibleData(fungibleData.amount, fungibleData.currency.value),
              percentage
            )
        }
      )
  }

  final case class FungibleDataAndPercentage(fungibleData: FungibleData, percentage: BigDecimal)

  object FungibleDataAndPercentage {
    implicit val fungibleDataAndPercentageCodec: JsonCodec[FungibleDataAndPercentage] =
      DeriveJsonCodec.gen[FungibleDataAndPercentage]
  }

  final case class DailyTradeData(netReturn: BigDecimal, tradeCount: Int)

  object DailyTradeData {
    implicit val dailyTradeDataCodec: JsonCodec[DailyTradeData] = DeriveJsonCodec.gen[DailyTradeData]

    def apply(data: CJDailyTradeData): DailyTradeData =
      new DailyTradeData(data.netReturn.value.amount, data.tradeCount.value)
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

  final case class JournalEntry(notes: Option[String], tags: List[String], mistakes: List[String], scamStrategy: Option[String])

  object JournalEntry {
    implicit val journalEntryCodec: JsonCodec[JournalEntry] = DeriveJsonCodec.gen[JournalEntry]

    implicit class JournalEntryOps(entry: JournalEntry) {
      def toDomainModel: CJJournalEntry =
        CJJournalEntry(
          entry.notes,
          tags = entry.tags.map(refineV[TagPredicate](_)).rights,
          mistakes = entry.mistakes.map(refineV[MistakePredicate](_)).rights,
          scamStrategy = entry.scamStrategy.flatMap(ScamStrategy(_).toOption)
        )
    }
  }

  implicit class DomainJournalEntryOps(entry: CJJournalEntry) {
    def toDto: JournalEntry =
      JournalEntry(entry.notes, entry.tags.map(_.value), entry.mistakes.map(_.value), entry.scamStrategy.map(_.toString))
  }

  final case class PositionJournalEntry(positionId: String, entry: JournalEntry)

  object PositionJournalEntry {
    implicit val positionTagsCodec: JsonCodec[PositionJournalEntry] = DeriveJsonCodec.gen[PositionJournalEntry]

    implicit class PositionTagsOps(posJournalEntry: PositionJournalEntry) {
      //TODO Add validation.
      def toDomainModel: CJPositionJournalEntry =
        CJPositionJournalEntry(
          positionId = refineV[PlayIdPredicate].unsafeFrom(posJournalEntry.positionId),
          entry = posJournalEntry.entry.toDomainModel,
        )
    }
  }

  final case class Ohlcv(
    timePeriodStart: Instant,
    timePeriodEnd: Instant,
    timeOpen: Instant,
    timeClose: Instant,
    priceOpen: FungibleData,
    priceHigh: FungibleData,
    priceLow: FungibleData,
    priceClose: FungibleData
  )

  object Ohlcv {
    implicit val ohlcvEntryCodec: JsonCodec[Ohlcv] = DeriveJsonCodec.gen[Ohlcv]
    def apply(ohlcv: CJOhlcv): Ohlcv =
      new Ohlcv(
        ohlcv.timePeriodStart,
        ohlcv.timePeriodEnd,
        ohlcv.timeOpen,
        ohlcv.timeClose,
        ohlcv.priceOpen.asJson,
        ohlcv.priceHigh.asJson,
        ohlcv.priceLow.asJson,
        ohlcv.priceClose.asJson
      )
  }
}
