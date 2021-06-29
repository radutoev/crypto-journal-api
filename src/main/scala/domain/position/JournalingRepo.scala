package io.softwarechain.cryptojournal
package domain.position

import domain.model.UserId
import domain.position.error.JournalSaveError
import domain.position.Position.PositionId

import zio.IO

trait JournalingRepo {
  def saveEntry(userId: UserId, positionId: PositionId, entry: JournalEntry): IO[JournalSaveError, Unit]
}
