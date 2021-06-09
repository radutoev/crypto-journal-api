package io.softwarechain.cryptojournal
package infrastructure.covalent

import infrastructure.covalent.model.{Buy, Sell, Transaction}

import zio.json._
import zio.test.Assertion._
import zio.test._

import java.time.{Instant, ZonedDateTime}
import scala.io.Source

object PositionGeneratorSpec extends DefaultRunnableSpec {
  override def spec = suite("PositionGeneratorSpec") {
    test("Closed position from transactions") {
      val accept = readFile("/covalent/accept.json").fromJson[Transaction].right.get
      val sell = readFile("/covalent/sell.json").fromJson[Transaction].right.get
      val buy = readFile("/covalent/buy.json").fromJson[Transaction].right.get

      val position = findPosition(List(accept, sell, buy))

      assert(position)(equalTo(Some(Position(Closed))))
    }
  }

  final case class Position(state: State)

  sealed trait State
  final case object Open extends State
  final case object Closed extends State

  val TransactionTypes = Vector(Buy, Sell)

  def findPosition(transactions: List[Transaction]): Option[Position] = {
    val chronologicalTransactions = transactions
      .filter(tx => tx.successful && TransactionTypes.contains(tx.transactionType))
      .sortBy(_.instant)(Ordering[Instant])

    chronologicalTransactions
      .lastOption
      .map(tx => tx.transactionType match {
        case Sell => Closed
        case _ => Open
      })
      .map(Position)
  }

  private def readFile(src: String) = {
    val source = Source.fromURL(getClass.getResource(src))
    val rawJsonString = source.mkString
    source.close()
    rawJsonString
  }
}
