package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{ FungibleData => CJFungibleData }
import domain.portfolio.model.PlaysGrouping
import vo.TimeInterval
import vo.filter.{ Count, KpiFilter }

import zhttp.http.URL
import zio.json.{ DeriveJsonCodec, JsonCodec }
import zio.prelude.Validation

import java.time.{ LocalDate, ZoneId, ZoneOffset }

object common {

  trait QParamsOps {
    def getInt(key: String, default: Int)(qParams: Map[String, String]): Validation[String, Int] =
      if (qParams.contains(key)) {
        Validation.fromOption(qParams(key).toIntOption).mapError(_ => "Query param has to be an integer")
      } else {
        Validation.succeed(default)
      }
  }

  implicit class IntervalQParamsOps(url: URL) {
    def intervalFilter(): Validation[String, TimeInterval] = {
      val qParams      = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }
      val rawStartDate = qParams.getOrElse("startdate", "")
      val rawEndDate   = qParams.getOrElse("enddate", "")

      try {
        val start = LocalDate.parse(rawStartDate).atStartOfDay(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        val end   = LocalDate.parse(rawEndDate).atStartOfDay(ZoneId.of(ZoneOffset.UTC.getId)).toInstant
        Validation.succeed(TimeInterval(start, end))
      } catch {
        case _: Exception => Validation.fail("Invalid time interval")
      }
    }
  }

  implicit class CountQParamOps(url: URL) extends QParamsOps {
    def countFilter(): Validation[String, Count] = {
      val qParams = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }
      if (qParams.contains("count")) {
        Validation
          .fromOption(qParams("count").toIntOption)
          .flatMap(Count.make)
          .mapError(_ => "Invalid count value")
      } else {
        Validation.fail("Count not provided")
      }
    }
  }

  implicit class KpiQParamsOps(url: URL) {
    def kpiFilter(): Validation[String, KpiFilter] =
      Validation.validateWith(
        Validation.succeed(url.countFilter().fold[Option[Count]](_ => None, Some(_))),
        Validation.succeed(url.intervalFilter().fold[Option[TimeInterval]](_ => None, Some(_)))
      ) {
        case (maybeCount, maybeInterval) =>
          KpiFilter(maybeCount, maybeInterval)
      }

    def playsGrouping(): Validation[String, Set[PlaysGrouping]] = {
      val rawGrouping = url.queryParams
        .get("grouping")
        .flatMap(_.headOption)
        .getOrElse("hour") //default to hour,

      val groupings = rawGrouping.split("[,]").map(PlaysGrouping.fromString).collect {
        case Right(grouping) => grouping
      }
      if (groupings.nonEmpty) {
        Validation.succeed(groupings.toSet)
      } else {
        Validation.fail("Invalid grouping param provided")
      }
    }
  }

  object dto {
    final case class FungibleData(amount: String, currency: String)

    object FungibleData {
      implicit val fungibleDataCodec: JsonCodec[FungibleData] = DeriveJsonCodec.gen[FungibleData]

      def apply(funData: CJFungibleData): FungibleData =
        new FungibleData(funData.amount.toString(), funData.currency.value)
    }

    implicit class FungibleDataOps(data: CJFungibleData) {
      def asJson: FungibleData = FungibleData(data)
    }

    implicit class OptionalFungibleDataOps(data: Option[CJFungibleData]) {
      def asJson: Option[FungibleData] = data.map(_.asJson)
    }
  }
}
