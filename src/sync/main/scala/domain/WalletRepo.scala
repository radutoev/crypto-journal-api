package io.softwarechain.cryptojournal
package domain

import domain.model.WalletAddress

import zio.{ Has, Ref, UIO, ZIO, ZLayer }

trait WalletRepo {
  protected def walletCache: Ref[Set[WalletAddress]]

  def exists(address: WalletAddress): UIO[Boolean]

  def addWallet(address: WalletAddress): UIO[Unit]

  def removeWallet(address: WalletAddress): UIO[Unit]
}

final case class LiveWalletRepo(initialWallets: Ref[Set[WalletAddress]]) extends WalletRepo {
  override protected def walletCache: Ref[Set[WalletAddress]] = initialWallets

  override def exists(address: WalletAddress): UIO[Boolean] =
    walletCache.get.map(wallets => wallets.contains(address))

  override def addWallet(address: WalletAddress): UIO[Unit] =
    for {
      wallets    <- walletCache.get
      newWallets = wallets + address
      _          <- walletCache.set(newWallets)
    } yield ()

  override def removeWallet(address: WalletAddress): UIO[Unit] =
    for {
      wallets    <- walletCache.get
      newWallets = wallets - address
      _          <- walletCache.set(newWallets)
    } yield ()
}

object LiveWalletRepo {
  lazy val layer: ZLayer[Any, Nothing, Has[WalletRepo]] = {
    (for {
      cache <- Ref.make(Set.empty[WalletAddress])
    } yield LiveWalletRepo(cache)).toLayer
  }

  def addWallet(address: WalletAddress): ZIO[Has[WalletRepo], Nothing, Unit] =
    ZIO.serviceWith[WalletRepo](_.addWallet(address))
}
