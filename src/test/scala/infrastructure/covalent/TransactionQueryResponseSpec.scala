package io.softwarechain.cryptojournal
package infrastructure.covalent

import dto.TransactionQueryResponse

import zio.json._
import zio.test.Assertion._
import zio.test._

import scala.io.Source

object TransactionQueryResponseSpec extends DefaultRunnableSpec {
  def spec = suite("TransactionQueryResponse")(
    test("json to dto") {
      val source        = Source.fromURL(getClass.getResource("/covalent/paginated_response.json"))
      val rawJsonString = source.mkString
      source.close()

      val transaction = rawJsonString.fromJson[TransactionQueryResponse]
      assert(transaction.isRight)(equalTo(true))
    }
  )
}
