package io.softwarechain.cryptojournal.infrastructure.covalent

import io.softwarechain.cryptojournal.infrastructure.covalent.dto.TransactionQueryResponse
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
    },
    //we fail for now on polymorphic param values.
    test("expecting ', found [") {
      val source        = Source.fromURL(getClass.getResource("/covalent/decoded_value_arr.json"))
      val rawJsonString = source.mkString
      source.close()

      val transaction = rawJsonString.fromJson[TransactionQueryResponse]
      assert(transaction.isRight)(equalTo(true))
    }
  )
}
