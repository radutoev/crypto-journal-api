package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.Transaction
import domain.model._

import LivePositionService.findPositions
import eu.timepit.refined
import zio.json._
import zio.test.Assertion.{equalTo, hasSameElementsDistinct}
import zio.test.{DefaultRunnableSpec, assert}

import java.time.Instant
import scala.io.Source
import scala.util.Try

object PositionGeneratorSpec extends DefaultRunnableSpec {
  override def spec = suite("PositionGeneratorSpec")(
    test("No position if insufficient tranactions") {
      assert(findPositions(List(readFile("/covalent/accept.json").fromJson[Transaction].right.get)))(
        equalTo(List.empty)
      )
    },
    test("Generate positions from multiple coins and transaction types") {
      val file         = readFile("/covalent/allTransactions.json").fromJson[List[Transaction]]
      val transactions = file.right.get

      val positions = findPositions(transactions)

      val expected = ExpectedData.map { row =>
        val parts = row.split("[;]")
        Position(
          refined.refineV[CurrencyPredicate].unsafeFrom(parts(0)),
          Instant.parse(parts(2)),
          List.empty
        )
      }

      assert(positions.size)(equalTo(18)) &&
      //I set empty entries so as not to initialize everything in this test. They are tested elsewhere.
      assert(positions.map(_.copy(entries = List.empty)))(hasSameElementsDistinct(expected))
    }
  )

  val ExpectedData = List(
    "DGBLACK;0x81d32fe0b6e39dd55c7c2ba4b69d9500a8efb28190f46e3c747b925dde10756b,0xee099c694ce69c15dd9b5a5cf17c279faf8aedb764ae19618da985e1c7d247ee;2021-05-22T10:24:05Z;2021-05-22T11:03:17Z",
    "BULLDOGE;0xeb37f026dd2d2911003c697757cf78dd47c2ac64f4bb5f9f338b714b744b7566,0x363604da4619d7b885f01d1efdf2224ed2dfaa88f7d3b435c6fe12632b841a02;2021-06-09T10:04:54Z;2021-06-09T10:13:45Z",
    "GNS;0x7fca938d64727aff042b5a104907bbcd79824cb54aaba22a68e9a0edab6f1d38,0x70fe14752152c7448a5f87f44fa6c9d97fe68f6d3cb5564ee730949c69583912;2021-05-22T07:21:37Z;2021-05-22T07:25:28Z",
    "SD;0x2cfff6271130bee9c3cca60e7de5744486ba7734beef75ff9f8845f369a350cb,0x28c21d9ebd61aa4532e0b1097342e413a33512206d04ae42f31b2f863445660a;2021-05-21T17:53:31Z;2021-05-21T17:59:13Z",
    "SCR;0x7109b75cf454ca866d0589ee93cd574bc9d8e1bedf8daa53e26e2c14121100e0;2021-05-21T13:10:45Z;;",
    "ORNG;0xc4787fe93f6751f82824abc63f5fed3e6576e8fa3bf34de9d4060b7314363935;2021-05-22T09:05:05Z;;",
    "INTO;0xaab1049e82aa84a23dd434816c05de2c9d03f77d9166e2d4aabc07bb72a2e15f,0xb7ca8e68fe49854abda01f54f4a89a0acc599e1fbde3423a4ce9e46be8e57d2d,0x55fc3c9ca1381a39187d3220954ea2338ec9eac5ebbb7a815ad18e559e5ffea6,0x22104a05668026278bf4171a693b69044f94ee1aab1532526ba545d6686295ee;2021-05-21T15:07:43Z;2021-05-21T15:56:54Z",
    "BULLRUN;0xd59e77a245826c3c87f9dc78ae285df9283274f8c7664a8eb08380cc87a64453,0xcb0d06d721d905be2e73df762fd029af690020b5cbe97441372033b35d3e0232;2021-05-24T16:13:04Z;2021-06-09T09:59:27Z",
    "FootballStars;0xe191803222fdf91da94af8bace6942a4e918e7b202d5fe5911fb2e92f54968d5,0x05b84a346079662d43006fb03e9d162164005dcaa035c722f9f497cb0d3b4544,0x02a775c17ff2c2910c9c809c033ff39fa28462747bf118cfec334d45af69a0e7;2021-05-21T20:40:36Z;2021-05-21T21:06:50Z",
    "EDOGE;0x85f8e28fcbbad0124c8d81a18ae09093bd3e577908ff8420a621230c88cd9c15,0xc920e623567ccbce73cc0cd1e68d0290101de318f65bf2c279c4054a6107872a,0x2da3b4f8c40bc31ace1c2b477940c7ccfc70385c2f8485d417d083528db063f7,0x6e73e390820ea46dd6bcb7913c31cf7e95522f8b3130b14c3c878ccb0b1f8fbc;2021-05-25T02:07:01Z;2021-05-25T03:08:55Z",
    "EDOGE;0xcf1c56bf171fe8c9b277c2f85a50923ba7faa61ff378bc4ca821bafd98b1f294,0xa90af84dfb215d960679028d027950178e4d8dd192a4bc700114be789f0c8618;2021-05-25T08:10:36Z;2021-05-25T08:19:30Z",
    "TIMELOCK;0x2e54693b62eab57c350131009caff23d0137e7af4a71d082c178c13620990c35,0xbcedde35ae21a893e9e838b925d3f4041355740fba59c42b15404852083f0d32,0x3ff0b471a227c723013f983b5ab61ee0704e5111552814a770d77a195e837529;2021-05-24T14:50:38Z;2021-05-24T15:14:35Z",
    "BTCPIZZA;0xf0003aaa4813e135b162ecbd5667c6daf1ac53e84c49a9a34cd46f2734d6e2aa,0x7e2e9eafb793f8ca683b90dcaf6d2d60cb50b39287964692b313a238fb54b8f2;2021-05-24T12:49:03Z;2021-05-24T12:50:15Z",
    "BTCPIZZA;0x7d2203dedce8280705926633c39d20078143176557ca76f54ebdda7780fb8b71,0xb5a59ac80a7c57403626f0c1717f29c5d7cbc76a545dc79f9eaf36883290176e;2021-05-24T12:50:54Z;2021-05-24T13:16:12Z",
    "$EMONEY;0x917d7398345343b0e9ada549e8e2e2bc1e3228e4ca96b2c6d1478094a411b46f,0x40c016916d84f78244a2b607f3eb04e8a04305867905c6547a0645e02e6813b4;2021-05-21T10:27:47Z;2021-05-21T10:37:14Z",
    "SAFEMOON;0x273edffb81d6f23649e29e72f4a08d5a5bcddfd97c56c5316ff93e00c1bbe41f;2021-05-26T12:33:16Z;;",
    "WonShiba;0x5c04b411451b5b95aee1225a1f0ba3c735bf5e75e0a24a23bf3cd43097ada110,0xca603b956568274072b989182ee9416536d9af127b397c2baf0805b5fd087b3d;2021-06-09T09:24:36Z;2021-06-09T09:34:24Z",
    "HSK;0x86944832aa9d99d1929b2363c22634563f03f7fdea052b1099b408936cfc70c7,0x5d82b164fd29664f6891e2d159f5f190b85633a173eb710dfc20a48f84233010;2021-06-09T09:40:36Z;2021-06-09T09:47:12Z"
  )

  private def readFile(src: String) = {
    val source        = Source.fromURL(getClass.getResource(src))
    val rawJsonString = source.mkString
    source.close()
    rawJsonString
  }
}
