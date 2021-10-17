package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ Mistake, Tag }
import domain.portfolio.model.{ FungibleDataPerformance, Performance }

final case class TagDistribution(
  mistakes: Map[Mistake, FungibleDataPerformance],
  tags: Map[Tag, FungibleDataPerformance]
)

object TagDistribution {
  def apply(portfolioKpi: PortfolioKpi, comparisonPortfolio: PortfolioKpi): TagDistribution = {
    val mistakeContribution = portfolioKpi.mistakeContribution
    val tagContribution     = portfolioKpi.tagContribution

    TagDistribution(
      mistakes = mistakeContribution.map {
        case (mistake, fungibleData) =>
          mistake -> FungibleDataPerformance(
            fungibleData,
            comparisonPortfolio.mistakeContribution
              .get(mistake)
              .map(data =>
                Performance(
                  absolute = fungibleData.difference(data).getOrElse(BigDecimal(0)),
                  percentage = fungibleData.percentageDifference(data).getOrElse(BigDecimal(0))
                )
              )
          )
      },
      tags = tagContribution.map {
        case (tag, fungibleData) =>
          tag -> FungibleDataPerformance(
            fungibleData,
            comparisonPortfolio.tagContribution
              .get(tag)
              .map(data =>
                Performance(
                  absolute = fungibleData.difference(data).getOrElse(BigDecimal(0)),
                  percentage = fungibleData.percentageDifference(data).getOrElse(BigDecimal(0))
                )
              )
          )
      }
    )
  }
}
