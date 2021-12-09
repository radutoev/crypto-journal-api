package io.softwarechain.cryptojournal
package domain.pricequote

import domain.model.{ Currency, FungibleData }

import java.time.Instant

final case class PriceQuotes(private val source: Map[CurrencyPair, List[PriceQuote]]) extends AnyVal {

  def findPrice(pair: CurrencyPair, timestamp: Instant): Option[PriceQuote] = {
    quotedValue(FungibleData(1, pair.base), pair.quote, timestamp)
      .map(data => PriceQuote(data.amount.toDouble, timestamp))
  }

  def quotedValue(value: FungibleData, quote: Currency, timestamp: Instant): Option[FungibleData] = {
    @inline
    def latestQuote(quotes: List[PriceQuote]): Option[PriceQuote] =
      quotes.filter(q => q.timestamp.isBefore(timestamp) || q.timestamp == timestamp).lastOption

    @inline
    def matchingBasePairs(lookup: CurrencyPair, basePool: Set[CurrencyPair]): Option[CurrencyPair] =
      basePool.find(b => b.quote == lookup.base)

    val pair = CurrencyPair(value.currency, quote)
    if (source.contains(pair)) {
      source.get(pair).flatMap(latestQuote)
        .map(quote => LinkedPriceQuotes(List(pair -> quote)).compute(value.amount))
        .map(value => FungibleData(value, quote))
    } else {
      val quotesAsBase = source.keySet.filter(p => p.base == pair.base)
      val targetPairs = source.keySet.collect {
        //TODO Memoize function - https://michid.wordpress.com/2009/02/23/function_mem/
        case p if p.quote == pair.quote && matchingBasePairs(p, quotesAsBase).isDefined =>
          val base = matchingBasePairs(p, quotesAsBase).get
          (base, p)
      }

      if (targetPairs.nonEmpty) {
        val (basePair, quotePair) = targetPairs.head
        (for {
          basePriceQuote  <- source.get(basePair).flatMap(latestQuote)
          quotePriceQuote <- source.get(quotePair).flatMap(latestQuote)
        } yield LinkedPriceQuotes(List(basePair -> basePriceQuote, quotePair -> quotePriceQuote)).compute(value.amount))
          .map(value => FungibleData(value, quote))
      } else {
        None
      }
    }
  }

  def isEmpty(): Boolean =
    source.isEmpty || source.map {
      case (_, quotes) => quotes.isEmpty
    }.forall(x => x)

  def nonEmpty(): Boolean = !isEmpty()
}

object PriceQuotes {
  def empty(): PriceQuotes = new PriceQuotes(Map.empty)

  def apply(quotes: Map[CurrencyPair, List[PriceQuote]]) =
    new PriceQuotes(quotes)

  def apply(pair: CurrencyPair, priceQuotes: List[PriceQuote]) = new PriceQuotes(Seq(pair -> priceQuotes).toMap)
}

private[pricequote] final case class LinkedPriceQuotes(private val list: List[(CurrencyPair, PriceQuote)]) {
  def compute(amount: BigDecimal): BigDecimal =
    if (list.nonEmpty) {
      list.foldLeft(amount)((acc, x) => acc * x._2.price)
    } else {
      BigDecimal(0)
    }
}

object LinkedPriceQuotes {
  private[pricequote] def apply(source: List[(CurrencyPair, PriceQuote)]): LinkedPriceQuotes =
    new LinkedPriceQuotes(source)
}
