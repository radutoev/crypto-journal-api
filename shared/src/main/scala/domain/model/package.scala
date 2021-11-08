package io.softwarechain.cryptojournal
package domain

import eu.timepit.refined.api.Refined
import eu.timepit.refined.boolean.And
import eu.timepit.refined.collection.{ NonEmpty, Size }
import eu.timepit.refined.generic.Equal
import eu.timepit.refined.numeric.NonNegative
import eu.timepit.refined.string.MatchesRegex

package object model {
  val USD: Currency  = Currency.unsafeFrom("USD")
  val WBNB: Currency = Currency.unsafeFrom("WBNB")

  type CoinAddressPredicate = NonEmpty
  type CoinAddress = String Refined CoinAddressPredicate

  type CurrencyPredicate = NonEmpty
  type Currency          = String Refined CurrencyPredicate

  type NumberOfDaysPredicate = NonNegative
  type NumberOfDays          = Int Refined NumberOfDaysPredicate

  type Fee = FungibleData

  type TransactionHashPredicate = NonEmpty
  type TransactionHash          = String Refined TransactionHashPredicate

  type WalletAddressPredicate = And[Size[Equal[42]], MatchesRegex["0x[a-zA-Z0-9]{40}"]]
  type WalletAddress          = String Refined WalletAddressPredicate

  type UserIdPredicate = NonEmpty
  type UserId          = String Refined UserIdPredicate

  type ContextIdPredicate = NonEmpty
  type ContextId          = String Refined ContextIdPredicate

  type TradeCountPredicate = NonNegative
  type TradeCount          = Int Refined TradeCountPredicate

  type TagPredicate = NonEmpty
  type Tag          = String Refined TagPredicate

  type MistakePredicate = NonEmpty
  type Mistake          = String Refined MistakePredicate

  type Percentage = BigDecimal

  type PlayIdPredicate = NonEmpty
  type PlayId          = String Refined PlayIdPredicate
}
