package io.softwarechain.cryptojournal
package infrastructure.google.pubsub

import domain.wallet.event.WalletEvent
import infrastructure.api.dto.Wallet

import zio.json.{ DeriveJsonCodec, JsonCodec }

object json {
  implicit val walletCodec: JsonCodec[Wallet] = DeriveJsonCodec.gen[Wallet]

  implicit val walletEventCodec: JsonCodec[WalletEvent] = DeriveJsonCodec.gen[WalletEvent]
}
