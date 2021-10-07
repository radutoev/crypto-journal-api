package io.softwarechain.cryptojournal
package domain.wallet

object model {
  sealed trait WalletImportState
  final case object Importing    extends WalletImportState
  final case object ImportDone   extends WalletImportState
  final case object ImportFailed extends WalletImportState

  object WalletImportState {
    def unsafeApply(value: String): WalletImportState = value.toLowerCase match {
      case "Importing"    => Importing
      case "ImportDone"   => ImportDone
      case "ImportFailed" => ImportFailed
    }
  }
}
