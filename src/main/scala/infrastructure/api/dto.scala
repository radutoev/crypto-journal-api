package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{FungibleData => CJFungibleData}
import domain.position.{Position => CJPosition, PositionEntry => CJPositionEntry}
import domain.portfolio.{PortfolioKpi => CJPorfolioKpi}
import domain.pricequote.{PriceQuote => CJPriceQuote, PriceQuotes}
import domain.wallet.{Wallet => CJWallet}

import zio.json.{DeriveJsonCodec, JsonCodec}

import java.time.Instant

object dto {
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
     win: Option[String],
     entries: List[PositionEntry]
  )

  final case class PositionEntry(
    `type`: String,
    value: FungibleData,
    fiatValue: Option[FungibleData],
    fee: FungibleData,
    fiatFee: Option[FungibleData]
  )

  final case class FungibleData(amount: BigDecimal, currency: String)

  final case class PriceQuote(price: Float, timestamp: Instant)

  object Position {
    implicit val priceQuoteCodec: JsonCodec[PriceQuote] = DeriveJsonCodec.gen[PriceQuote]
    implicit val feeCodec: JsonCodec[FungibleData]            = DeriveJsonCodec.gen[FungibleData]
    implicit val positionEntryCodec: JsonCodec[PositionEntry] = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]           = DeriveJsonCodec.gen[Position]

    def fromPosition(position: CJPosition): Position =
      Position(
        position.currency,
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
        position.win().map(isWin => if(isWin) "win" else "lose"),
        position.entries.map(entry => fromPositionEntry(entry)(position.priceQuotes.getOrElse(PriceQuotes.empty())))
      )

    def fromPositionEntry(entry: CJPositionEntry)(implicit priceQuotes: PriceQuotes): PositionEntry = {
      PositionEntry(
        entry.`type`.toString,
        entry.value.asJson,
        entry.fiatValue.map(_.asJson),
        entry.fee.asJson,
        entry.fiatFee.map(_.asJson)
      )
    }
  }

  final case class Wallet(userId: String, address: String)

  object Wallet {
    implicit val walletCodec: JsonCodec[Wallet] = DeriveJsonCodec.gen[Wallet]

    def fromWallet(wallet: CJWallet): Wallet = {
      Wallet(wallet.userId.value, wallet.address.value)
    }
  }

  implicit class FungibleDataOps(data: CJFungibleData) {
    def asJson: FungibleData = FungibleData(data.amount, data.currency)
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

  final case class PortfolioKpi(tradeCount: Int, winRate: Float, looseRate: Float, netReturn: BigDecimal)

  object PortfolioKpi {
    implicit val kpiCodec: JsonCodec[PortfolioKpi] = DeriveJsonCodec.gen[PortfolioKpi]

    def apply(kpi: CJPorfolioKpi): PortfolioKpi = {
      PortfolioKpi(kpi.tradeCount, kpi.winRate, 1 - kpi.winRate, kpi.netReturn.amount)
    }
  }
}
