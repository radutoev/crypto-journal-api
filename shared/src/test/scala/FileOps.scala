package io.softwarechain.cryptojournal

import infrastructure.covalent.dto.Transaction

import zio.json.DecoderOps

import scala.io.Source

trait FileOps {
  def getTransaction(file: String) =
    getTransactions(file).head

  def getTransactions(file: String) = {
    val f = readFile(file).fromJson[List[Transaction]]
    f.right.get.map(tx => tx.toDomain())
  }

  def readFile(src: String) = {
    val source        = Source.fromURL(getClass.getResource(src))
    val rawJsonString = source.mkString
    source.close()
    rawJsonString
  }
}
