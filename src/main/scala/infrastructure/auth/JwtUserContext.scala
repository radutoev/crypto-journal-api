package io.softwarechain.cryptojournal
package infrastructure.auth

import domain.account.UserContext
import domain.model.UserId

import zio.{Has, UIO, ULayer, ZLayer}

final case class JwtUserContext(id: UserId) extends UserContext {
  override def userId: UIO[UserId] = UIO(id)
}

object JwtUserContext {
  def layer(userId: UserId): ULayer[Has[UserContext]] = ZLayer.fromFunction(_ => JwtUserContext apply userId)
}
