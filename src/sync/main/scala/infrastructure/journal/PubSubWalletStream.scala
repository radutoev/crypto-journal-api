package io.softwarechain.cryptojournal
package infrastructure.journal

import domain.model.WalletAddress
import domain.wallet.Wallet
import infrastructure.journal.dto.{WalletAdded, WalletUpdate}

import eu.timepit.refined.types.string.NonEmptyString
import zio.stream.ZStream

object PubSubWalletStream {
  def walletStream(): ZStream[Any, Nothing, WalletUpdate] =
    ZStream.fromIterable(
      List(
        WalletAdded(
          Wallet(
            NonEmptyString.unsafeFrom("xxx"),
            WalletAddress.unsafeApply("0xB142db79A25b05e5a2F1daA39B67A2Dd2FCb96ce")
          )
        )
      )
    )
}
