package io.softwarechain.cryptojournal
package domain.position

object model {
  sealed trait ScamStrategy
  final case object ExcludeFromStats extends ScamStrategy
  final case object NoOp extends ScamStrategy

  object ScamStrategy {
    def apply(value: String): Either[String, ScamStrategy] = {
      value.toLowerCase.trim match {
        case "excludefromstats" => Right(ExcludeFromStats)
        case "noop" => Right(NoOp)
        case _ => Left(s"Unknown scam strategy $value")
      }
    }
  }
}
