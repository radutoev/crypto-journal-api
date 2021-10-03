package io.softwarechain.cryptojournal
package infrastructure.binance

import org.web3j.protocol.Web3j
import org.web3j.protocol.http.HttpService
import zio.interop.reactivestreams.publisherToStream
import zio.stream.ZStream
import zio.{ZIO, ZManaged}

object BnbListener {
  def positionEntryStream() = {
    def acquire()             = ZIO.succeed(Web3j.build(new HttpService("https://bsc-dataseed.binance.org")))
    def release(web3j: Web3j) = ZIO.succeed(web3j.shutdown())

    ZStream.managed(ZManaged.make(acquire())(release)).flatMap(web3j => web3j.blockFlowable(true).toStream())
  }
}
