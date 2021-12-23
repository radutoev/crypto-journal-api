package io.softwarechain.cryptojournal
package domain.position

import domain.model.{Currency, FungibleData, PlayId}
import domain.position.MarketPlay.MergeResult.{NoChange, NoMerge, PositionsMerged}
import vo.TimeInterval

import java.time.Instant

trait MarketPlay {
  def id: Option[PlayId]

  def openedAt: Instant

  def fees(): Map[Currency, FungibleData]

  def inInterval(interval: TimeInterval): Boolean = {
    val moreRecentThanStart = interval.start.isBefore(openedAt) || interval.start == openedAt
    val beforeEnd           = interval.end.isAfter(openedAt) || interval.end == openedAt
    moreRecentThanStart && beforeEnd
  }
}

object MarketPlay {
  def merge(older: MarketPlay, newer: MarketPlay): MergeResult = {
    newer match {
      case _ @ (_: TopUp | _: Withdraw) => NoMerge
      case p: Position                  =>
        older match {
          case _ @ (_: TopUp | _: Withdraw) => NoMerge
          case oldPosition: Position        => merge(oldPosition, p)
        }
    }
  }

  private def merge(older: Position, newer: Position): MergeResult = {
    if(older.isOpen) {
      newer.entries.headOption.fold[MergeResult](NoChange) { entry =>
        if(entry.isInstanceOf[Sell]) {
          PositionsMerged(older.addEntries(newer.entries))
        } else {
          NoMerge
        }
      }
    } else {
      PositionsMerged(older.addEntries(newer.entries))
    }
  }

  sealed trait MergeResult
  object MergeResult {
    final case object NoChange extends MergeResult
    final case class  PositionsMerged(newPosition: Position) extends MergeResult
    final case object NoMerge extends MergeResult
  }
}
