package io.softwarechain.cryptojournal
package domain.wallet

import domain.model.WalletAddress

import zio.{Has, Ref, UIO, ZIO, ZLayer}

trait WalletCache {
  protected def walletCache: Ref[Set[WalletAddress]]

  def exists(address: WalletAddress): UIO[Boolean]

  def addWallet(address: WalletAddress): UIO[Unit]

  def addWallets(addresses: Set[WalletAddress]): UIO[Unit]

  def removeWallet(address: WalletAddress): UIO[Unit]
}

object WalletCache {
  def addWallet(address: WalletAddress): ZIO[Has[WalletCache], Nothing, Unit] =
    ZIO.serviceWith[WalletCache](_.addWallet(address))

  def exists(address: WalletAddress): ZIO[Has[WalletCache], Nothing, Boolean] =
    ZIO.serviceWith[WalletCache](_.exists(address))
}

final case class LocalWalletCache(initialWallets: Ref[Set[WalletAddress]]) extends WalletCache {
  override protected def walletCache: Ref[Set[WalletAddress]] = initialWallets

  override def exists(address: WalletAddress): UIO[Boolean] =
    walletCache.get.map(wallets => wallets.contains(address))

  override def addWallet(address: WalletAddress): UIO[Unit] =
    for {
      wallets    <- walletCache.get
      newWallets = wallets + address
      _          <- walletCache.set(newWallets)
    } yield ()

  override def addWallets(addresses: Set[WalletAddress]): UIO[Unit] =
    ZIO.foreach_(addresses)(address => addWallet(address))

  override def removeWallet(address: WalletAddress): UIO[Unit] =
    for {
      wallets    <- walletCache.get
      newWallets = wallets - address
      _          <- walletCache.set(newWallets)
    } yield ()
}

object LocalWalletCache {
  lazy val layer: ZLayer[Any, Nothing, Has[WalletCache]] = {
    (for {
      cache <- Ref.make(Set.empty[WalletAddress])
    } yield LocalWalletCache(cache)).toLayer
  }
}