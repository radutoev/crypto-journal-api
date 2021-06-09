package io.softwarechain.cryptojournal

import zhttp.http._
import zhttp.service.Server
import zio.{App, ExitCode, URIO}

object CryptoJournal extends App {

  val app = Http.collect[Request] {
    case Method.GET -> Root / "hello" => Response.text("Hello, world!")
  }

  override def run(args: List[String]): URIO[zio.ZEnv, ExitCode] =
    Server.start(8080, app).exitCode
}
