package io.softwarechain.cryptojournal
package infrastructure.auth

import domain.account.RequestContext
import domain.model

import io.softwarechain.cryptojournal.domain.model.{ContextId, UserId}
import zio.{Has, UIO, ULayer, ZLayer}

final case class JwtRequestContext(uId: UserId, cId: ContextId) extends RequestContext {
  override def userId: UIO[UserId] = UIO(uId)

  override def contextId: UIO[model.ContextId] = UIO(cId)
}

object JwtRequestContext {
  def layer(userId: UserId, contextId: ContextId): ULayer[Has[RequestContext]] =
    ZLayer.fromFunction(_ => JwtRequestContext(userId, contextId))
}
