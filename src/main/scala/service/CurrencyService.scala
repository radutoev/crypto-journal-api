package io.softwarechain.cryptojournal
package service

import service.LiveCurrencyService.HistoricWBNBToUSDMap

import zio.{Function0ToLayerSyntax, Has, Task, ULayer, ZIO}

import java.time.{Instant, ZoneId, ZoneOffset}

trait CurrencyService {
  //currently supports only WBNB to USD
  def convert(amount: BigDecimal, timestamp: Instant): Task[BigDecimal]
}

object CurrencyService {
  def convert(amount: BigDecimal, timestamp: Instant) =
    ZIO.serviceWith[CurrencyService](_.convert(amount, timestamp))
}

final case class LiveCurrencyService() extends CurrencyService {
  override def convert(amount: BigDecimal, timestamp: Instant): Task[BigDecimal] = Task {
    val lookup = timestamp
      .atZone(ZoneId.systemDefault())
      .toLocalDateTime
      .withHour(0).withMinute(0).withSecond(0)
      .toInstant(ZoneOffset.UTC)

    val unitUsdValue = BigDecimal(HistoricWBNBToUSDMap(lookup))
    unitUsdValue * amount
  }
}

object LiveCurrencyService {
  lazy val layer: ULayer[Has[CurrencyService]] = (LiveCurrencyService.apply _).toLayer

  /* This just handles WBNB to USD conversion. */
  val HistoricWBNBToUSDMap = Map(
    Instant.parse("2021-06-09T00:00:00Z") -> 375.246833,
    Instant.parse("2021-06-08T00:00:00Z") -> 353.68415,
    Instant.parse("2021-06-07T00:00:00Z") -> 359.659433,
    Instant.parse("2021-06-06T00:00:00Z") -> 394.647625,
    Instant.parse("2021-06-05T00:00:00Z") -> 389.05378,
    Instant.parse("2021-06-04T00:00:00Z") -> 394.371615,
    Instant.parse("2021-06-03T00:00:00Z") -> 429.195772,
    Instant.parse("2021-06-02T00:00:00Z") -> 404.131167,
    Instant.parse("2021-06-01T00:00:00Z") -> 361.873519,
    Instant.parse("2021-05-31T00:00:00Z") -> 352.506733,
    Instant.parse("2021-05-30T00:00:00Z") -> 325.652062,
    Instant.parse("2021-05-29T00:00:00Z") -> 307.493209,
    Instant.parse("2021-05-28T00:00:00Z") -> 330.649594,
    Instant.parse("2021-05-27T00:00:00Z") -> 368.530089,
    Instant.parse("2021-05-26T00:00:00Z") -> 379.788052,
    Instant.parse("2021-05-25T00:00:00Z") -> 341.489341,
    Instant.parse("2021-05-24T00:00:00Z") -> 344.013879,
    Instant.parse("2021-05-23T00:00:00Z") -> 263.810768,
    Instant.parse("2021-05-22T00:00:00Z") -> 305.750562,
    Instant.parse("2021-05-21T00:00:00Z") -> 329.14904,
  )
}
