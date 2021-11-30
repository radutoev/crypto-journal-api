package io.softwarechain.cryptojournal
package application

import domain.currency.CurrencyRepo
import domain.position.error.MarketPlayError
import domain.pricequote.PriceQuoteService
import infrastructure.google.datastore.DatastorePaginationRepo

import zio.clock.Clock
import zio.duration.durationInt
import zio.{Has, Schedule, ZIO}

object SyncApi {
  def syncPriceQuotes(): ZIO[Has[CurrencyRepo] with Has[PriceQuoteService] with Clock, RuntimeException, Long] = {
    ZIO.services[CurrencyRepo, PriceQuoteService].flatMap { case (currencyRepo, priceQuoteService) =>
      (for {
        currencies <- currencyRepo.getCurrencies()
        _          <- priceQuoteService.updateQuotes(currencies)
      } yield ()).orElseFail(new RuntimeException("Error updating quotes"))
    } repeat Schedule.spaced(1.hour)
  }

  def clearPaginationContext(): ZIO[Has[DatastorePaginationRepo] with Clock, MarketPlayError, Unit] = {
    (ZIO.serviceWith[DatastorePaginationRepo](_.cleanup()) repeat Schedule.spaced(1.day)).unit
  }
}
