package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{FungibleData => CJFungibleData}
import domain.portfolio.{PortfolioKpi => CJPortfolioKpi}
import domain.position.{JournalEntry => CJJournalEntry, Position => CJPosition, PositionEntry => CJPositionEntry}
import domain.pricequote.{PriceQuotes, PriceQuote => CJPriceQuote}
import domain.wallet.{Wallet => CJWallet}
import vo.JournalPosition

import eu.timepit.refined.types.string.NonEmptyString
import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.{Duration, Instant}

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
    entryPrice: Option[PriceQuote],
    exitPrice: Option[PriceQuote],
    numberOfExecutions: Int,
    holdTime: Option[Long],
    win: Option[Boolean],
    entries: List[PositionEntry],
    id: Option[String],
    journalEntry: Option[JournalEntry]
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

    def fromJournalPosition(journalPosition: JournalPosition): Position =
      fromPosition(journalPosition.position).copy(journalEntry = journalPosition.entry.map(_.toDto))

    def fromPosition(position: CJPosition): Position =
      Position(
        position.currency.value,
        position.state.toString,
        position.openedAt,
        position.closedAt,
        position.totalCost().asJson,
        position.totalFees().asJson,
        position.fiatReturn().asJson,
        position.totalCoins().asJson,
        position.entryPrice().asJson,
        position.exitPrice().asJson,
        position.numberOfExecutions(),
        position.holdTime,
        position.isWin().map(isWin => if (isWin) true else false),
        position.entries.map(entry => fromPositionEntry(entry)(position.priceQuotes.getOrElse(PriceQuotes.empty()))),
        position.id.map(_.value),
        None
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
    accountBalance: BigDecimal,
    tradeCount: Int,
    winRate: Float,
    loseRate: Float,
    netReturn: BigDecimal,
    balanceTrend: List[BigDecimal]
  )

  object PortfolioKpi {
    implicit val portfolioCodec: JsonCodec[PortfolioKpi] = DeriveJsonCodec.gen[PortfolioKpi]

    def apply(kpi: CJPortfolioKpi): PortfolioKpi =
      new PortfolioKpi(
        kpi.balance.amount,
        kpi.tradeCount,
        kpi.winRate,
        1 - kpi.winRate,
        kpi.netReturn.amount,
        kpi.balanceTrend.map(_.amount)
      )
  }

  final case class KpiDistinctValues(netReturn: BigDecimal,
                                     biggestWin: Option[BigDecimal],
                                     biggestLoss: Option[BigDecimal],
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
                                     totalFees: BigDecimal)

  object KpiDistinctValues {
    implicit val kpiDistinctCodec: JsonCodec[KpiDistinctValues] = DeriveJsonCodec.gen[KpiDistinctValues]

    def asHumanReadableForm(d: Duration): String = {
      d.toString.substring(2).replaceAll("(\\d[HMS])(?!$)", "$1 ").toLowerCase()
    }

    def apply(portfolio: CJPortfolioKpi): KpiDistinctValues =
      new KpiDistinctValues(
        portfolio.netReturn.amount,
        portfolio.biggestWin.map(_.amount),
        portfolio.biggestLoss.map(_.amount),
        portfolio.winRate,
        loseRate = 1 - portfolio.winRate,
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
        portfolio.totalFees.amount
      )
  }

  final case class JournalEntry(notes: Option[String], setups: List[String], mistakes: List[String])

  object JournalEntry {
    implicit val journalEntryCodec: JsonCodec[JournalEntry] = DeriveJsonCodec.gen[JournalEntry]

    implicit class JournalEntryOps(entry: JournalEntry) {
      def toDomainModel: CJJournalEntry =
        CJJournalEntry(
          entry.notes.map(NonEmptyString.unsafeFrom),
          setups = entry.setups.map(NonEmptyString.unsafeFrom),
          mistakes = entry.mistakes.map(NonEmptyString.unsafeFrom)
        )
    }
  }

  implicit class DomainJournalEntryOps(entry: CJJournalEntry) {
    def toDto: JournalEntry =
      JournalEntry(entry.notes.map(_.value), entry.setups.map(_.value), entry.mistakes.map(_.value))
  }
}
