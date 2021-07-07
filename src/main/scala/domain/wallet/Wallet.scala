package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.{ UserId, WalletAddress }

final case class Wallet(userId: UserId, address: WalletAddress)
