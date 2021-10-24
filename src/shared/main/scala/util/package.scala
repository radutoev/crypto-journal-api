package io.softwarechain.cryptojournal

import domain.position.{MarketPlay, Position, TransferIn, TransferOut}

import com.google.cloud.Timestamp

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

    def toLocalDate(): LocalDate =
      LocalDate.ofInstant(instant, ZoneId.of(ZoneOffset.UTC.getId))

    def toDatastoreTimestamp(): Timestamp =
      Timestamp.ofTimeSecondsAndNanos(instant.getEpochSecond, instant.getNano)
  }

  implicit class ListOps[Err, Value](list: List[Either[Err, Value]]) {
    lazy val rights: List[Value] = list.collect {
      case Right(value) => value
    }
  }

  implicit class MarketPlaysListOps(marketPlays: List[MarketPlay]) {
    lazy val positions: List[Position] = marketPlays.collect { case p: Position => p }

    lazy val transferIns: List[TransferIn] = marketPlays.collect { case t: TransferIn => t }

    lazy val transferOuts: List[TransferOut] = marketPlays.collect { case t: TransferOut => t }

    def mostRecentFirst(): List[MarketPlay] = marketPlays.sortBy(_.openedAt)(Ordering[Instant].reverse)
  }

  val BeginningOfCrypto: Instant = Instant.parse("2017-07-01T00:00:00.000Z") //June 2017 Binance launched.

  implicit class EitherOps[Left, Left2, Right](either: Either[Left, Right]) {
    def mapLeft(left: Left => Left2): Either[Left2, Right] = either.left.map(left)
  }

  def tryOrLeft[T, E](f: => T, fail: E): Either[E, T] =
    Try(f).toEither.left.map(_ => fail)
}
