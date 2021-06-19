package io.softwarechain.cryptojournal
package domain.position

object error {
  sealed trait PositionError
  final case class InvalidRepresentation(message: String) extends PositionError
}
