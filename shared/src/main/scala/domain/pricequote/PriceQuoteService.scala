package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.Currency
import domain.pricequote.error.PriceQuoteError
import vo.TimeInterval

import zio.IO

trait PriceQuoteService {
  def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]]
}

final case class LivePriceQuoteService(priceQuoteRepo: PriceQuoteRepo) extends PriceQuoteService {
  override def getQuotes(currencies: Set[Currency], timeInterval: TimeInterval): IO[PriceQuoteError, Map[Currency, PriceQuotes]] = {
    priceQuoteRepo.getQuotes(currencies, timeInterval).map(_.map { case (currency, items) => currency -> PriceQuotes(items) })
  }
}