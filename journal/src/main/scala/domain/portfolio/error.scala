package io.softwarechain.cryptojournal
package domain.portfolio

object error {
  sealed trait PortfolioError

  final case class InvalidPortfolioError(reason: String)        extends PortfolioError
  final case class PortfolioKpiGenerationError(message: String) extends PortfolioError
}
