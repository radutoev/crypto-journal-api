package io.softwarechain.cryptojournal
package domain.account

import domain.model.{ContextId, UserId}

import zio.{Has, UIO, ZIO}

trait RequestContext {
  def userId: UIO[UserId]

  def contextId: UIO[ContextId]
}

object RequestContext {
  def userId: ZIO[Has[RequestContext], Nothing, UserId] = ZIO.serviceWith(_.userId)

  def contextId: ZIO[Has[RequestContext], Nothing, ContextId] = ZIO.serviceWith(_.contextId)
}
