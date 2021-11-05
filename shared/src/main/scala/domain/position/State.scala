package io.softwarechain.cryptojournal
package domain.position

sealed trait State
final case object Open   extends State
final case object Closed extends State

object State {
  def apply(value: String): Either[String, State] =
    value.trim.toLowerCase match {
      case "open"   => Right(Open)
      case "closed" => Right(Closed)
      case _        => Left(s"Invalid state representation: $value")
    }
}
