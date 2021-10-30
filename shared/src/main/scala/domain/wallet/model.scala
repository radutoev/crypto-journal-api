package io.softwarechain.cryptojournal
package domain.wallet

object model {
  sealed trait WalletImportStatus
  final case object Importing    extends WalletImportStatus
  final case object ImportDone   extends WalletImportStatus
  final case object ImportFailed extends WalletImportStatus

  object WalletImportStatus {
    def unsafeApply(value: String): WalletImportStatus = value.toLowerCase match {
      case "Importing"    => Importing
      case "ImportDone"   => ImportDone
      case "ImportFailed" => ImportFailed
    }
  }
}
