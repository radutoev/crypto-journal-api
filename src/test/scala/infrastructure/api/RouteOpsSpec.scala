package io.softwarechain.cryptojournal
package infrastructure.api

import infrastructure.api.Routes.PositionsQParamsOps
import vo.PositionFilter

import zhttp.http._
import zio.test._
import zio.test.Assertion._

object RouteOpsSpec extends DefaultRunnableSpec {
  override def spec = suite("Positions query params spec")(
    test("extract positions count") {
      assert(URL(Path("test"), queryParams = Map("count" -> List("4"))).positionFilter())(equalTo(PositionFilter(4)))
    },
    test("default to 30 if no count provided") {
      assert(URL(Path("test")).positionFilter())(equalTo(PositionFilter(30)))
    },
    test("map first encountered param value") {
      assert(URL(Path("test"), queryParams = Map("count" -> List("7", "4"))).positionFilter())(equalTo(PositionFilter(7)))
    },
    testM("fail if negative value provided") {
      check(Gen.int(Int.MinValue, -1)) {
        paramValue =>
          assert(URL(Path("test"), queryParams = Map("count" -> List(paramValue.toString))).positionFilter().toEither)(
            isLeft
          )
      }
    },
    testM("fail if alphanumeric") {
      check(Gen.anyString) {
        paramValue =>
          assert(URL(Path("test"), queryParams = Map("count" -> List(paramValue))).positionFilter().toEither)(
            isLeft
          )
      }
    },
    testM("fail if float") {
      check(Gen.anyFloat) {
        paramValue =>
          assert(URL(Path("test"), queryParams = Map("count" -> List(paramValue.toString))).positionFilter().toEither)(
            isLeft
          )
      }
    }
  )
}
