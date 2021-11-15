package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{ MistakePredicate, PlayIdPredicate, TagPredicate, FungibleData => CJFungibleData }
import domain.market.{ Ohlcv => CJOhlcv }
import domain.portfolio.model.{ DailyTradeData => CJDailyTradeData, Performance => CJPerformance }
import domain.portfolio.{ AccountBalance, NetReturn, PortfolioKpi => CJPortfolioKpi }
import domain.position.{
  JournalEntry => CJJournalEntry,
  MarketPlay => CJMarketPlay,
  Position => CJPosition,
  PositionEntry => CJPositionEntry,
  PositionJournalEntry => CJPositionJournalEntry,
  TopUp => CJTopUp,
  Withdraw => CJWithdraw
}
import domain.position.model.ScamStrategy
import domain.pricequote.{ PriceQuote => CJPriceQuote }
import domain.wallet.{ Wallet => CJWallet }
import vo.filter.Count
import vo.{ PeriodDistribution => CJPeriodDistribution }
import infrastructure.api.dto.MarketPlay._
import util.ListEitherOps

import eu.timepit.refined.refineV
import domain.position

import zio.json.{ DeriveJsonCodec, JsonCodec }

import java.time.{ Duration, Instant }

object dto {
  sealed trait MarketPlay

  final case class TopUp(
    hash: String,
    value: FungibleData,
    fees: Map[String, FungibleData],
    timestamp: Instant,
    id: Option[String]
  ) extends MarketPlay

  final case class Withdrawal(
    hash: String,
    value: FungibleData,
    fees: Map[String, FungibleData],
    timestamp: Instant,
    id: Option[String]
  ) extends MarketPlay

  final case class Position(
    currency: String,
    address: Option[String],
    state: String,
    openedAt: Instant,
    closedAt: Option[Instant],
    costs: Map[String, FungibleData],
    fees: Map[String, FungibleData],
    fiatReturn: Option[FungibleData],
    totalCoins: FungibleData,
    orderSize: BigDecimal,
    averageOrderSize: BigDecimal,
    entryPrice: Option[PriceQuote],
    exitPrice: Option[PriceQuote],
    numberOfExecutions: Int,
    holdTime: Option[Long],
    win: Option[Boolean],
    entries: List[PositionEntry],
    id: Option[String],
    journalEntry: JournalEntry
  ) extends MarketPlay

  sealed trait PositionEntry
  final case class AirDrop(
    receivedFrom: String,
    fee: FungibleData,
    received: FungibleData,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class Approval(
    fee: FungibleData,
    forContract: String,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class Buy(
    fee: FungibleData,
    spent: FungibleData,
    received: FungibleData,
    coinAddress: String,
    hash: String,
    timestamp: Instant,
    spentOriginal: Option[FungibleData],
    id: Option[String]
  ) extends PositionEntry
  final case class Claim(
    fee: FungibleData,
    received: FungibleData,
    receivedFrom: String,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class Contribute(
    spent: FungibleData,
    to: String,
    fee: FungibleData,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class Sell(
    sold: FungibleData,
    received: FungibleData,
    fee: FungibleData,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class TransferIn(
    value: FungibleData,
    receivedFrom: String,
    fee: FungibleData,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry
  final case class TransferOut(
    amount: FungibleData,
    to: String,
    fee: FungibleData,
    hash: String,
    timestamp: Instant,
    id: Option[String]
  ) extends PositionEntry

  final case class FungibleData(amount: String, currency: String)

  object FungibleData {
    def apply(funData: CJFungibleData): FungibleData = {
      new FungibleData(funData.amount.toString(), funData.currency.value)
    }
  }

  final case class PriceQuote(price: Float, timestamp: Instant)

  object MarketPlay {
    implicit val priceQuoteCodec: JsonCodec[PriceQuote]     = DeriveJsonCodec.gen[PriceQuote]
    implicit val fungibleDataCodec: JsonCodec[FungibleData] = DeriveJsonCodec.gen[FungibleData]

    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]

    implicit val transferInPlayCodec: JsonCodec[TopUp]  = DeriveJsonCodec.gen[TopUp]
    implicit val marketPlayCodec: JsonCodec[MarketPlay] = DeriveJsonCodec.gen[MarketPlay]

    def fromMarketPlay(m: CJMarketPlay): MarketPlay =
      m match {
        case pos: CJPosition  => fromPosition(pos)
        case t: CJTopUp       => fromTopUp(t)
        case tOut: CJWithdraw => fromWithdrawal(tOut)
      }

    def fromPosition(position: CJPosition): Position =
      Position(
        position.currency.map(_.value).getOrElse(""),
        position.coinAddress.map(_.value),
        position.state.toString,
        position.openedAt,
        position.closedAt,
        position.cost.map { case (currency, data) => currency.value -> data.asJson },
        position.fees.map { case (currency, data) => currency.value -> data.asJson },
        position.fiatReturn.asJson,
        position.totalCoins.asJson,
        position.orderSize,
        position.averageOrderSize,
        position.entryPrice.asJson,
        position.exitPrice.asJson,
        position.numberOfExecutions,
        position.holdTime,
        position.isWin.map(isWin => if (isWin) true else false),
        position.entries.map(entry => fromPositionEntry(entry)),
        position.id.map(_.value),
        position.journal.map(_.toDto).getOrElse(JournalEntry(None, List.empty, List.empty, None))
      )

    def fromPositionEntry(entry: CJPositionEntry): PositionEntry =
      entry match {
        case position.AirDrop(receivedFrom, fee, received, hash, timestamp, id) =>
          AirDrop(
            receivedFrom.value,
            FungibleData(fee),
            FungibleData(received),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Approval(fee, forContract, hash, timestamp, id) =>
          Approval(
            FungibleData(fee),
            forContract.value,
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Buy(fee, spent, received, coinAddress, hash, timestamp, spentOriginal, id) =>
          Buy(
            FungibleData(fee),
            FungibleData(spent),
            FungibleData(received),
            coinAddress.value,
            hash.value,
            timestamp,
            spentOriginal.map(f => FungibleData(f)),
            id.map(_.value)
          )
        case position.Claim(fee, received, receivedFrom, hash, timestamp, id) =>
          Claim(
            FungibleData(fee),
            FungibleData(received),
            receivedFrom.value,
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Contribute(spent, to, fee, hash, timestamp, id) =>
          Contribute(
            FungibleData(spent),
            to.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Sell(sold, received, fee, hash, timestamp, id) =>
          Sell(
            FungibleData(sold),
            FungibleData(received),
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.TransferIn(value, receivedFrom, fee, hash, timestamp, id) =>
          TransferIn(
            FungibleData(value),
            receivedFrom.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.TransferOut(amount, to, fee, hash, timestamp, id) =>
          TransferOut(
            FungibleData(amount),
            to.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
      }

    private def fromTopUp(t: CJTopUp): TopUp =
      TopUp(
        hash = t.txHash.value,
        value = t.value.asJson,
        fees = t.fees.map { case (currency, fee) => currency.value -> fee.asJson },
        timestamp = t.timestamp,
        id = t.id.map(_.value)
      )

    private def fromWithdrawal(t: CJWithdraw): Withdrawal =
      Withdrawal(
        hash = t.txHash.value,
        value = t.value.asJson,
        fees = t.fees.map { case (currency, fee) => currency.value -> fee.asJson },
        timestamp = t.timestamp,
        id = t.id.map(_.value)
      )
  }

  final case class Wallet(userId: String, address: String)

  object Wallet {
    implicit val walletCodec: JsonCodec[Wallet] = DeriveJsonCodec.gen[Wallet]

    def fromWallet(wallet: CJWallet): Wallet =
      Wallet(wallet.userId.value, wallet.address.value)
  }

  implicit class FungibleDataOps(data: CJFungibleData) {
    def asJson: FungibleData = FungibleData(data)
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

  final case class JournalEntry(
    notes: Option[String],
    tags: List[String],
    mistakes: List[String],
    scamStrategy: Option[String]
  )

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
      JournalEntry(
        entry.notes,
        entry.tags.map(_.value),
        entry.mistakes.map(_.value),
        entry.scamStrategy.map(_.toString)
      )
  }

  final case class PositionJournalEntry(positionId: String, entry: JournalEntry)

  object PositionJournalEntry {
    implicit val positionTagsCodec: JsonCodec[PositionJournalEntry] = DeriveJsonCodec.gen[PositionJournalEntry]

    implicit class PositionTagsOps(posJournalEntry: PositionJournalEntry) {
      //TODO Add validation.
      def toDomainModel: CJPositionJournalEntry =
        CJPositionJournalEntry(
          positionId = refineV[PlayIdPredicate].unsafeFrom(posJournalEntry.positionId),
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
