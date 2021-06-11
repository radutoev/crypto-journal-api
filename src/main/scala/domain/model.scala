package io.softwarechain.cryptojournal
package domain

import infrastructure.covalent.model.{Buy, Sell}

object model {
  sealed trait State
  final case object Open extends State
  final case object Closed extends State

  val TransactionTypes = Vector(Buy, Sell)
}
