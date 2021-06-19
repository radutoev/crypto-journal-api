package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain._
import domain.model.{UserId, WalletAddress}

import zio.{Function1ToLayerSyntax, Has, Task, URLayer, ZIO}
import zio.json._
import zio.logging.Logging

import java.time.Instant
import scala.collection.mutable.ArrayBuffer
import scala.io.Source

trait PositionRepo {
//  def getPositions(wallet: String): Task[List[Position]]

  def save(userId: UserId, address: WalletAddress, positions: List[Position]): Task[Unit]
}

object PositionRepo {
  def save(userId: UserId, address: WalletAddress, positions: List[Position]): ZIO[Has[PositionRepo], Throwable, Unit] =
    ZIO.serviceWith[PositionRepo](_.save(userId, address, positions))

//  def getPositions(wallet: String): ZIO[Has[PositionRepo], Throwable, List[Position]] =
//    ZIO.serviceWith[PositionRepo](_.getPositions(wallet))
}

//final case class LivePositionRepo(ethBlockchainRepo: EthBlockchainRepo) extends PositionRepo {
//  override def getPositions(wallet: String): Task[List[Position]] =
//    Task {
//      val source        = Source.fromURL(getClass.getResource("/allTransactions.json"))
//      val rawJsonString = source.mkString
//      source.close()
//      val either = rawJsonString.fromJson[List[Transaction]]
//      either.right.get
//    }.map(findPositions)
////    ethBlockchainRepo.fetchTransactions(wallet).map(findPositions)
//}
