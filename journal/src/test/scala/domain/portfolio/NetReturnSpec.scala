package io.softwarechain.cryptojournal
package domain.portfolio

import domain.position.model.ExcludeFromStats
import domain.position.{JournalEntry, MarketPlays}

import zio.test.Assertion._
import zio.test._

object NetReturnSpec extends DefaultRunnableSpec {
  override def spec = suite("NetReturn")(
    testM("Exclude scams marked for exclusions") {
      check(Gen.listOf(Generators.genClosedPosition)) { positions =>
        val testSource = if(positions.nonEmpty) {
          positions.head.copy(journal = journalEntryWithExcludeStrategy()) +: positions.tail
        } else positions

        val expectedNetReturn = if(positions.nonEmpty) {
          positions.tail.map(_.fiatReturn()).sumFungibleData()
        } else positions.map(_.fiatReturn()).sumFungibleData()

        assert(expectedNetReturn.amount)(equalTo(NetReturn(MarketPlays(testSource)).value.amount))
      }
    }
  )

  private def journalEntryWithExcludeStrategy(): Option[JournalEntry] =
    Some(
      JournalEntry(
        notes = None,
        tags = List.empty,
        mistakes = List.empty,
        scamStrategy = Some(ExcludeFromStats)
      )
    )
}
