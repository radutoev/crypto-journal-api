package io.softwarechain.cryptojournal

import domain.model.{ Currency, FungibleData, TransactionHashPredicate, TransactionType, Unknown }
import domain.position.{ Position, PositionEntry }

import eu.timepit.refined.refineV
import eu.timepit.refined.types.string.NonEmptyString
import zio.random.Random
import zio.test.{ Gen, Sized }

object Generators {
//  val genPositionId: Gen[Random, NonEmptyString] =
//    Gen.stringBounded(2, 100)(Gen.alphaNumericChar).map(NonEmptyString.unsafeFrom)
//  val genCurrency: Gen[Random with Sized, Currency] = Gen.stringN(10)(Gen.anyChar).map(NonEmptyString.unsafeFrom)
//  val genEntryType: Gen[Random, TransactionType]    = Gen.boolean.map(_ => Unknown)
//  val genTxHash                                     = Gen.stringBounded(2, 100)(Gen.alphaNumericChar).map(refineV[TransactionHashPredicate].unsafeFrom(_))
//
//  val genFungibleData = for {
//    amount   <- Gen.anyDouble.map(BigDecimal(_))
//    currency <- genCurrency
//  } yield FungibleData(amount, currency)
//
//  val genPositionEntry = for {
//    txType    <- genEntryType
//    timestamp <- Gen.anyInstant
//    value     <- genFungibleData
//    fee       <- genFungibleData
//    txHash    <- genTxHash
//  } yield PositionEntry(txType, value, fee, timestamp, txHash, None)
//
//  val genClosedPositionEntry = genPositionEntry//.map(entry => entry.copy(`type` = Sell))
//
//  val genOpenPositionEntry = genPositionEntry//.map(entry => entry.copy(`type` = Buy))
//
//  val genPosition = for {
//    positionId <- genPositionId
//    currency   <- genCurrency
//    openedAt   <- Gen.anyInstant
//    entries    <- Gen.listOfN(2)(genPositionEntry)
//  } yield Position(currency, openedAt, entries, id = Some(positionId))
//
//  val genClosedPosition = for {
//    positionId <- genPositionId
//    currency   <- genCurrency
//    openedAt   <- Gen.anyInstant
//    entries    <- Gen.listOfN(2)(genPositionEntry)
//    closed     <- genClosedPositionEntry
//  } yield Position(currency, openedAt, entries :+ closed, id = Some(positionId))
}
