package io.softwarechain.cryptojournal
package domain.position

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.refineV

object model {
  sealed trait ScamStrategy
  final case object HideFromStats extends ScamStrategy
  final case object DoNothing     extends ScamStrategy

  object ScamStrategy {
    def apply(value: String): Either[String, ScamStrategy] =
      value.toLowerCase.trim match {
        case "hidefromstats" => Right(HideFromStats)
        case "donothing"     => Right(DoNothing)
        case _               => Left(s"Unknown scam strategy $value")
      }
  }

  type CoinNamePredicate = NonEmpty
  type CoinName = String Refined CoinNamePredicate

  object CoinName {
    def unsafeApply(s: String): CoinName = refineV[CoinNamePredicate].unsafeFrom(s)
  }
}
