package io.softwarechain.cryptojournal
package vo

import domain.model.ContextId

import eu.timepit.refined.api.Refined
import eu.timepit.refined.collection.NonEmpty
import eu.timepit.refined.types.numeric.PosLong

object pagination {
  type CursorPredicate = NonEmpty
  type Cursor          = String Refined CursorPredicate

  //TODO This does look like an entity more than a VO, but I'm not sure where to fit it in the domain yet.
  final case class PaginationContext(contextId: ContextId, cursor: Cursor, filterHash: PosLong)

  final case class Page[T](data: T, contextId: Option[ContextId])
}
