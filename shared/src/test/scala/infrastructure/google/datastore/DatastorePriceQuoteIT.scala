package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.date.{ DayUnit, HourUnit, MinuteUnit }
import domain.model.{ BUSD, WBNB }
import domain.pricequote.{ CurrencyPair, PriceQuote, PriceQuoteRepo }
import vo.{ PriceQuotesChunk, TimeInterval }

import com.google.cloud.datastore.DatastoreOptions
import zio.clock.Clock
import zio.logging.slf4j.Slf4jLogger
import zio.test.Assertion.equalTo
import zio.test.TestAspect.ifPropSet
import zio.test.{ assert, DefaultRunnableSpec }
import zio.{ Has, UIO, ZIO, ZLayer }

import java.time.Instant

object DatastorePriceQuoteIT extends DefaultRunnableSpec {
  override def spec =
    suite("Price Quotes Integration Tests")(
      testM("No price quotes if chunk is empty") {
        val pair = CurrencyPair(WBNB, BUSD)
        for {
          _      <- PriceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, List.empty))
          now    <- ZIO.accessM[Clock](_.get.instant)
          quotes <- PriceQuoteRepo.getQuotes(pair, TimeInterval(now.minusSeconds(3600), now), DayUnit)
        } yield assert(quotes)(equalTo(Nil))
      },
      testM("Save distinct days and hours") {
        val pair = CurrencyPair(WBNB, BUSD)
        val interval =
          TimeInterval(Instant.parse("2021-10-10T00:00:00.000Z"), Instant.parse("2021-10-11T20:10:00.000Z"))
        for {
          _            <- PriceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, DistinctDaysAndHours))
          dayQuotes    <- PriceQuoteRepo.getQuotes(pair, interval, DayUnit)
          hourlyQuotes <- PriceQuoteRepo.getQuotes(pair, interval, HourUnit)
          minuteQuotes <- PriceQuoteRepo.getQuotes(pair, interval, MinuteUnit)
        } yield assert(dayQuotes.size)(equalTo(2)) && assert(hourlyQuotes.size)(equalTo(3)) && assert(minuteQuotes.size)(equalTo(4))
      },
      testM("Upsert day") {
        val pair = CurrencyPair(WBNB, BUSD)
        val interval =
          TimeInterval(Instant.parse("2021-10-12T00:00:00.000Z"), Instant.parse("2021-10-12T23:10:00.000Z"))
        for {
          _      <- PriceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, InitialData))
          _      <- PriceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, UpdateData))
          quotes <- PriceQuoteRepo.getQuotes(pair, interval, DayUnit)
        } yield assert(quotes.size)(equalTo(1)) && assert(quotes.head)(
          equalTo(PriceQuote(3.5d, Instant.parse("2021-10-12T00:00:00.000Z")))
        )
      }
    ).provideCustomLayerShared(layer) @@ ifPropSet("DATASTORE_EMULATOR_HOST")

  val layer: ZLayer[Clock, Nothing, Has[PriceQuoteRepo]] =
    UIO(DatastoreOptions.getDefaultInstance.toBuilder.build().getService).toLayer ++
      UIO(
        DatastoreConfig(
          address = "AddressTest",
          journal = "JournalTest",
          marketPlay = "MarketPlayTest",
          paginationContext = "PaginationContextTest",
          priceQuote = "PriceQuoteTest",
          userWallet = "UserWalletTest",
          wallet = "WalletTest"
        )
      ).toLayer ++
      ZLayer.requires[Clock] ++
      Slf4jLogger.make((_, m) => m) >>> DatastorePriceQuoteRepo.layer

  val DistinctDaysAndHours = List(
    PriceQuote(2d, Instant.parse("2021-10-10T15:10:00.000Z")),
    PriceQuote(2d, Instant.parse("2021-10-10T15:25:00.000Z")),
    PriceQuote(2d, Instant.parse("2021-10-10T16:25:00.000Z")),
    PriceQuote(2d, Instant.parse("2021-10-11T16:25:00.000Z"))
  )

  val InitialData = List(
    PriceQuote(2d, Instant.parse("2021-10-12T16:25:00.000Z"))
  )

  val UpdateData = List(
    PriceQuote(5d, Instant.parse("2021-10-12T18:25:00.000Z"))
  )
}
