package io.softwarechain.cryptojournal
package infrastructure.api

import domain.model.{ FungibleData => CJFungibleData }
import vo.TimeInterval
import vo.filter.Count

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
