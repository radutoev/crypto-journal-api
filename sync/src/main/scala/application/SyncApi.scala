package io.softwarechain.cryptojournal
package application

import domain.currency.CurrencyRepo
import domain.model.WalletAddress
import domain.pricequote.PriceQuoteService
import infrastructure.bitquery.BitQueryFacade
import infrastructure.google.datastore.DatastorePaginationRepo

import zio.clock.Clock
import zio.duration.durationInt
import zio.{ Has, Schedule, UIO, URIO, ZIO }

object SyncApi {
  def syncPriceQuotes() =
    ZIO.services[CurrencyRepo, PriceQuoteService].flatMap {
      case (currencyRepo, priceQuoteService) =>
        (for {
          data <- BitQueryFacade.getPrices(
                   WalletAddress.unsafeFrom("0xd25631648e3ad4863332319e8e0d6f2a8ec6f267"),
                   WalletAddress.unsafeFrom("0xbb4CdB9CBd36B01bD1cBaEBF2De08d9173bc095c")
                 )
//          currencies <- currencyRepo.getCurrencies()
//          _          <- priceQuoteService.updateQuotes(currencies)
        } yield ()).orElseFail(new RuntimeException("Error updating quotes"))
    } repeat Schedule.spaced(1.hour)

  def clearPaginationContext(): URIO[Has[DatastorePaginationRepo] with Clock, Unit] =
    (ZIO.serviceWith[DatastorePaginationRepo](_.cleanup()) repeat Schedule.spaced(1.day)).unit.ignore
}
