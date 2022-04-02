package io.softwarechain.cryptojournal
package infrastructure.pricequote

import domain.model.Currency
import domain.model.date.{ Hour, TimeUnit }
import domain.pricequote.error.{ PriceQuoteError, PriceQuotesSaveError }
import domain.pricequote._
import infrastructure.bitquery.BitQueryFacade
import vo.{ PriceQuotesChunk, TimeInterval }

import zio.logging.{ Logger, Logging }
import zio.{ Has, IO, UIO, URLayer, ZIO }

final case class LivePriceQuoteService(
  bitQueryFacade: BitQueryFacade,
  priceQuoteRepo: PriceQuoteRepo,
  logger: Logger[String]
) extends PriceQuoteService {

  override def addQuote(pair: CurrencyAddressPair, hour: Hour): IO[PriceQuoteError, Unit] =
    (for {
      quotes <- bitQueryFacade.getPrices(pair, hour)
      _ <- logger.info(
            s"Found ${quotes.size} quotes for ${pair.base.currency} -> ${pair.quote.currency}  @ ${hour.value}"
          )
      cPair = CurrencyPair(pair.base.currency, pair.quote.currency)
      _     <- logger.info(s"Save price quotes ${pair.base.currency} -> ${pair.quote.currency} @ ${hour.value}")
      _     <- priceQuoteRepo.saveQuotes(PriceQuotesChunk(cPair, quotes))
    } yield ()).orElseFail(
      PriceQuotesSaveError(CurrencyPair(pair.base.currency, pair.quote.currency), "Unable to save price quotes")
    )

  override def getQuotes(pair: CurrencyAddressPair, interval: TimeInterval, unit: TimeUnit): IO[PriceQuoteError, PriceQuotes] = {
    val currencyPair = CurrencyPair(pair.base.currency, pair.quote.currency)
    for {
      repoQuotes <- priceQuoteRepo.getQuotes(currencyPair, interval, unit)
      quotes <- if (repoQuotes.isEmpty) {
                 logger.info(s"No $unit quotes found for $pair between $interval. Attempt to fetch again...") *>
                 ZIO.foreach_(interval.hours)(hour => addQuote(pair, hour)) *> priceQuoteRepo.getQuotes(
                   currencyPair,
                   interval,
                   unit
                 )
               } else UIO(repoQuotes)
      _ <- logger.warn(s"No quotes for $pair | $interval | $unit").when(quotes.isEmpty)
    } yield PriceQuotes(Map(currencyPair -> quotes))
  }

  def getQuotes(
    currencies: Set[Currency],
    targetCurrency: Currency,
    interval: TimeInterval,
    unit: TimeUnit
  ): IO[PriceQuoteError, PriceQuotes] =
    priceQuoteRepo
      .getQuotes(currencies, targetCurrency, interval, unit)
      .map(tuples => PriceQuotes(tuples.groupBy(_._1).view.mapValues(_.map(_._2)).toMap))
}

object LivePriceQuoteService {
  lazy val layer: URLayer[Has[BitQueryFacade] with Has[PriceQuoteRepo] with Logging, Has[PriceQuoteService]] =
    (LivePriceQuoteService(_, _, _)).toLayer
}
