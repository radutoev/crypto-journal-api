package io.softwarechain.cryptojournal
package domain.account

import domain.model.UserId

import zio.{Has, UIO, ZIO}

trait UserContext {
  def userId: UIO[UserId]
}

object UserContext {
  def userId: ZIO[Has[UserContext], Nothing, UserId] = ZIO.serviceWith(_.userId)
}

