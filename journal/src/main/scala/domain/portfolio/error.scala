package io.softwarechain.cryptojournal
package domain.portfolio

object error {
  sealed trait StatsError

  final case class InvalidPortfolioError(reason: String)        extends StatsError
  final case class AccountBalanceComputeError(reason: String)   extends StatsError
  final case class PortfolioKpiGenerationError(message: String) extends StatsError
}
