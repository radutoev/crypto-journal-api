package io.softwarechain.cryptojournal
package domain.portfolio

import domain.model.{ FungibleData, TradeCount }

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty

object model {
//  final case class DailyTradeData(netReturn: NetReturn, tradeCount: TradeCount)

//  type DayPredicate = MatchesRegex[W.`"""^\d{4,5}-(0[1-9]|1[0-2])-(0[1-9]|[12][0-9]|3[01])$"""`.T]
  type DayPredicate = NonEmpty
  type DayFormat    = String Refined DayPredicate
}
