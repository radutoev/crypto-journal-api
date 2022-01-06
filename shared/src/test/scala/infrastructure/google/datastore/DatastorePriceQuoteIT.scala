package io.softwarechain.cryptojournal
package infrastructure.google.datastore

import config.DatastoreConfig
import domain.model.date.DayUnit
import domain.model.{BUSD, WBNB}
import domain.pricequote.{CurrencyPair, PriceQuoteRepo}
import vo.{PriceQuotesChunk, TimeInterval}

import com.google.cloud.datastore.DatastoreOptions
import zio.clock.Clock
import zio.logging.slf4j.Slf4jLogger
import zio.test.Assertion.equalTo
import zio.test.TestAspect.ifPropSet
import zio.test.{DefaultRunnableSpec, assert}
import zio.{Has, UIO, ZIO, ZLayer}

object DatastorePriceQuoteIT extends DefaultRunnableSpec {
  override def spec = suite("Price Quotes Integration Tests")(
    testM("No price quotes if chunk is empty") {
      val pair = CurrencyPair(WBNB, BUSD)
      for {
        _      <- PriceQuoteRepo.saveQuotes(PriceQuotesChunk(pair, List.empty))
        now    <- ZIO.accessM[Clock](_.get.instant)
        quotes <- PriceQuoteRepo.getQuotes(pair, TimeInterval(now.minusSeconds(3600), now), DayUnit)
      } yield assert(quotes)(equalTo(Nil))
    }
  ).provideCustomLayerShared(layer)  @@ ifPropSet("DATASTORE_EMULATOR_HOST")

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
}
