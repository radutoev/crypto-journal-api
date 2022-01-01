package io.softwarechain.cryptojournal

import domain.position.{MarketPlay, Position, TopUp, Withdraw}

import com.google.cloud.Timestamp
import io.softwarechain.cryptojournal.domain.model.date.Hour

import java.time.{Instant, LocalDate, ZoneId, ZoneOffset}
import scala.util.Try

package object util {
  implicit class InstantOps(instant: Instant) {
    def atBeginningOfDay(): Instant =
      instant
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withHour(0)
        .withMinute(0)
        .withSecond(0)
        .toInstant(ZoneOffset.UTC)

    def atEndOfDay(): Instant =
      instant
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withHour(23)
        .withMinute(59)
        .withSecond(59)
        .toInstant(ZoneOffset.UTC)

    def atBeginningOfMinute(): Instant =
      instant
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withSecond(0)
        .toInstant(ZoneOffset.UTC)

    def atBeginningOfHour(): Instant =
      instant
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withSecond(0)
        .withMinute(0)
        .toInstant(ZoneOffset.UTC)

    def nextMinute(): Instant =
      instant
        .plusSeconds(60)
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withSecond(0)
        .toInstant(ZoneOffset.UTC)

    def toLocalDate(): LocalDate =
      LocalDate.ofInstant(instant, ZoneId.of(ZoneOffset.UTC.getId))

    def toDatastoreTimestamp(): Timestamp =
      Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
  }

  implicit class InstantsOps(instants: Set[Instant]) {
    //default to hour for now, as I don't need anything extra.
    lazy val distributionByHour: Map[Hour, Set[Instant]] = {
      instants.map(instant => instant.atBeginningOfHour() -> instant)
        .groupBy(_._1)
        .map { case (hourInstant, list) =>
          Hour(hourInstant) -> list.map(_._2)
        }
    }
  }

  object ListOps {
    def cond[T](p: => Boolean, v: () => T): List[T] = if (p) v.apply() :: Nil else Nil
  }

  implicit class ListEitherOps[Err, Value](list: List[Either[Err, Value]]) {
    lazy val rights: List[Value] = list.collect {
      case Right(value) => value
    }
  }

  implicit class ListOptionOps[Value](list: List[Option[Value]]) {
    lazy val values: List[Value] = list.collect {
      case Some(value) => value
    }
  }

  implicit class MarketPlaysListOps(marketPlays: List[MarketPlay]) {
    lazy val positions: List[Position] = marketPlays.collect { case p: Position => p }

    lazy val transferIns: List[TopUp] = marketPlays.collect { case t: TopUp => t }

    lazy val transferOuts: List[Withdraw] = marketPlays.collect { case t: Withdraw => t }

    def mostRecentFirst(): List[MarketPlay] = marketPlays.sortBy(_.openedAt)(Ordering[Instant].reverse)
  }

  val BeginningOfCrypto: Instant = Instant.parse("2017-07-01T00:00:00.000Z") //June 2017 Binance launched.

  implicit class EitherOps[Left, Left2, Right](either: Either[Left, Right]) {
    def mapLeft(left: Left => Left2): Either[Left2, Right] = either.left.map(left)
  }

  def tryOrLeft[T, E](f: => T, fail: E): Either[E, T] =
    Try(f).toEither.left.map(_ => fail)
}
