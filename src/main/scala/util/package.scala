package io.softwarechain.cryptojournal

import java.time.{ Instant, ZoneId, ZoneOffset }
import scala.util.Try

package object util {
  implicit class InstantOps(instant: Instant) {
    def resetHourAndMinute(): Instant =
      instant
        .atZone(ZoneId.of(ZoneOffset.UTC.getId))
        .toLocalDateTime
        .withHour(0)
        .withMinute(0)
        .withSecond(0)
        .toInstant(ZoneOffset.UTC)
  }

  implicit class EitherOps[Left, Left2, Right](either: Either[Left, Right]) {
    def mapLeft(left: Left => Left2) = either.left.map(left)
  }

  def tryOrLeft[T, E](f: => T, fail: E): Either[E, T] =
    Try(f).toEither.left.map(_ => fail)
}
