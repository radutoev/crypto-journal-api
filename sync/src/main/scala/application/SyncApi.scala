package io.softwarechain.cryptojournal
package application

import domain.pricequote.PriceQuotesJobService
import domain.pricequote.error.PriceQuoteError
import infrastructure.google.datastore.DatastorePaginationRepo

import zio.clock.Clock
import zio.duration.durationInt
import zio.{Has, Schedule, URIO, ZIO}

object SyncApi {
  def updatePriceQuotes(): ZIO[Has[PriceQuotesJobService] with Clock, PriceQuoteError, Long] =
    ZIO.serviceWith[PriceQuotesJobService](_.updateQuotes()) repeat Schedule.spaced(1.hour)

  def clearPaginationContext(): URIO[Has[DatastorePaginationRepo] with Clock, Unit] =
    (ZIO.serviceWith[DatastorePaginationRepo](_.cleanup()) repeat Schedule.spaced(1.day)).unit.ignore
}
