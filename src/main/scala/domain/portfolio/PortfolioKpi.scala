package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.FungibleData
import domain.position.Position

final class PortfolioKpi (positions: List[Position]) {
  private lazy val closedPositions = positions.filter(_.isClosed())

  lazy val tradeCount = closedPositions.size

  lazy val winRate: Float = {
    winRate(closedPositions)
  }

  lazy val netReturn: FungibleData = {
    val currency = closedPositions.head.currency
    val amount = closedPositions.map(_.fiatReturn().get.amount).sum
    FungibleData(amount, currency)
  }

  private def winRate(reference: List[Position]): Float = {
    val totalCount = reference.size
    val winCount = reference.count(position => {
      //.get is safe because win will be present on all closed positions.
      position.win().get
    })
    winCount / totalCount
  }
}
