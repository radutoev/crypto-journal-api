package io.softwarechain.cryptojournal
package infrastructure.api

import domain.market.{ Ohlcv => CJOhlcv }
import domain.model.{ MistakePredicate, TagPredicate, FungibleData => CJFungibleData }
import domain.portfolio.{ PortfolioKpi => CJPortfolioKpi, NetReturn => CJNetReturn }
import domain.position.{
  JournalEntry => CJJournalEntry,
  Position => CJPosition,
  PositionEntry => CJPositionEntry,
  PositionJournalEntry => CJPositionJournalEntry
}
import domain.position.Position.PositionIdPredicate
import domain.pricequote.{ PriceQuotes, PriceQuote => CJPriceQuote }
import domain.wallet.{ Wallet => CJWallet }
import vo.{ PeriodDistribution => CJPeriodDistribution }

import dto.Position._
import eu.timepit.refined.refineV
import zio.json.{ DeriveJsonCodec, JsonCodec }

import java.time.{ Duration, Instant }

object dto {
  final case class Positions(positions: List[Position], lastSync: Option[Instant])

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
  )

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

  object Position {
    implicit val priceQuoteCodec: JsonCodec[PriceQuote]       = DeriveJsonCodec.gen[PriceQuote]
    implicit val feeCodec: JsonCodec[FungibleData]            = DeriveJsonCodec.gen[FungibleData]
    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]

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
        position.journal.map(_.toDto).getOrElse(JournalEntry(None, List.empty, List.empty))
      )

    def fromPositionEntry(entry: CJPositionEntry)(implicit priceQuotes: PriceQuotes): PositionEntry =
      PositionEntry(
        entry.`type`.toString,
        entry.value.asJson,
        entry.fiatValue.map(_.asJson),
        entry.fee.asJson,
        entry.fiatFee.map(_.asJson),
        entry.timestamp,
        entry.id.map(_.value)
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
    accountBalance: FungibleData,
    tradeCount: Int,
    winRate: Float,
    loseRate: Float,
    netReturn: NetReturn,
    avgDailyTradeCount: Float,
    balanceTrend: List[BigDecimal],
    netReturnTrend: List[BigDecimal]
  )

  object PortfolioKpi {
    implicit val portfolioCodec: JsonCodec[PortfolioKpi] = DeriveJsonCodec.gen[PortfolioKpi]

    def apply(kpi: CJPortfolioKpi): PortfolioKpi =
      new PortfolioKpi(
        kpi.balance.asJson,
        kpi.tradeCount,
        kpi.winRate,
        kpi.loseRate,
        NetReturn(kpi.netReturn, CJNetReturn(kpi.referencePositions)),
        kpi.avgDailyTradeCount,
        kpi.balanceTrend.map(_.amount),
        kpi.netReturnTrend.map(_.amount)
      )
  }

  final case class NetReturn(value: FungibleData, trend: List[FungibleData], performance: String)

  object NetReturn {
    implicit val netReturnCodec: JsonCodec[NetReturn] = DeriveJsonCodec.gen[NetReturn]

    def apply(netReturn: CJNetReturn, compareWith: CJNetReturn): NetReturn = {
      new NetReturn(netReturn.value.asJson, netReturn.trend.map(_.asJson), netReturn.performance(compareWith).toString)
    }
  }

  final case class PortfolioStats(
    distinctValues: KpiDistinctValues,
    tradeSummary: TradeSummary,
    periodDistribution: PeriodDistribution,
    tagDistribution: TagDistribution
  )

  object PortfolioStats {
    implicit val portfolioStatsCodec: JsonCodec[PortfolioStats] = DeriveJsonCodec.gen[PortfolioStats]

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

  final case class CoinToFungiblePair(currency: String, fungibleData: FungibleData)

  object CoinToFungiblePair {
    implicit val xCodec: JsonCodec[CoinToFungiblePair] = DeriveJsonCodec.gen[CoinToFungiblePair]
  }

  final case class TradeSummary(wins: List[CoinToFungiblePair], loses: List[CoinToFungiblePair])

  object TradeSummary {
    implicit val tradeSummaryCodec: JsonCodec[TradeSummary] = DeriveJsonCodec.gen[TradeSummary]

    def apply(portfolio: CJPortfolioKpi): TradeSummary =
      new TradeSummary(
        wins = portfolio.coinWins.map(t => CoinToFungiblePair(t._1.value, t._2.asJson)),
        loses = portfolio.coinLoses.map(t => CoinToFungiblePair(t._1.value, t._2.asJson))
      )
  }

  final case class TagDistribution(mistakes: Map[String, FungibleData], tags: Map[String, FungibleData])

  object TagDistribution {
    implicit val tagDistributionCodec: JsonCodec[TagDistribution] = DeriveJsonCodec.gen[TagDistribution]

    def apply(portfolioKpi: CJPortfolioKpi): TagDistribution =
      new TagDistribution(mistakes = portfolioKpi.mistakeContributions.map {
        case (mistake, fungibleData) => mistake.value -> fungibleData.asJson
      }, tags = portfolioKpi.tagContribution.map {
        case (setup, fungibleData) => setup.value -> fungibleData.asJson
      })
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

  final case class JournalEntry(notes: Option[String], tags: List[String], mistakes: List[String])

  object JournalEntry {
    implicit val journalEntryCodec: JsonCodec[JournalEntry] = DeriveJsonCodec.gen[JournalEntry]

    implicit class JournalEntryOps(entry: JournalEntry) {
      def toDomainModel: CJJournalEntry =
        CJJournalEntry(
          entry.notes,
          tags = entry.tags.map(refineV[TagPredicate](_)).collect { case Right(setup)         => setup },
          mistakes = entry.mistakes.map(refineV[MistakePredicate](_)).collect { case Right(mistake) => mistake }
        )
    }
  }

  implicit class DomainJournalEntryOps(entry: CJJournalEntry) {
    def toDto: JournalEntry =
      JournalEntry(entry.notes, entry.tags.map(_.value), entry.mistakes.map(_.value))
  }

  final case class PositionJournalEntry(positionId: String, entry: JournalEntry)

  object PositionJournalEntry {
    implicit val positionTagsCodec: JsonCodec[PositionJournalEntry] = DeriveJsonCodec.gen[PositionJournalEntry]

    implicit class PositionTagsOps(posJournalEntry: PositionJournalEntry) {
      //TODO Add validation.
      def toDomainModel: CJPositionJournalEntry =
        CJPositionJournalEntry(
          positionId = refineV[PositionIdPredicate].unsafeFrom(posJournalEntry.positionId),
          entry = posJournalEntry.entry.toDomainModel
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
