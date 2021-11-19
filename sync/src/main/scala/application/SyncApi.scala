package io.softwarechain.cryptojournal
package application

import domain.currency.CurrencyRepo
import domain.pricequote.PriceQuoteService

import zio.{Has, ZIO}

object SyncApi {
  def syncPriceQuotes(): ZIO[Has[CurrencyRepo] with Has[PriceQuoteService], Throwable, Unit] = {
    ZIO.services[CurrencyRepo, PriceQuoteService].flatMap { case (currencyRepo, priceQuoteService) =>
      (for {
        currencies <- currencyRepo.getCurrencies()
        _          <- priceQuoteService.updateQuotes(currencies)
      } yield ()).orElseFail(new RuntimeException("Error updating quotes"))
    }
  }
}
