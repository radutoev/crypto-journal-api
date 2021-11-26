package io.softwarechain.cryptojournal
package domain.position

import domain.blockchain.error._
import domain.blockchain.{ BlockchainRepo, Transaction }
import domain.currency.CurrencyRepo
import domain.model._
import domain.position.MarketPlays.findMarketPlays
import domain.position.error._
import domain.pricequote.{ PriceQuote, PriceQuoteRepo, PriceQuotes }
import domain.wallet.Wallet
import util.{ InstantOps, ListOptionOps, MarketPlaysListOps }
import vo.TimeInterval
import vo.filter.PlayFilter

import zio.logging.{ Logger, Logging }
import zio.stream.ZStream
import zio.{ Has, IO, Task, UIO, URLayer, ZIO }

import java.time.Instant

trait MarketPlayService {
  def getPlays(userWallet: Wallet): IO[MarketPlayError, MarketPlays]

  def getPlays(userWallet: Wallet, filter: PlayFilter): IO[MarketPlayError, MarketPlays]

  def getPlays(userWallet: Wallet, filter: PlayFilter, contextId: ContextId): IO[MarketPlayError, MarketPlays]

  def getPosition(userId: UserId, playId: PlayId): IO[MarketPlayError, PositionDetails[Position]]

  def getNextPositions(playId: PlayId): IO[MarketPlayError, List[Position]]

  def getPreviousPositions(playId: PlayId): IO[MarketPlayError, List[Position]]

  def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit]

  def importPlays(userWallet: Wallet, startingFrom: Instant): IO[MarketPlayError, Unit]

  def extractTimeInterval(marketPlays: List[MarketPlay]): Option[TimeInterval] = {
    val timestamps = marketPlays.flatMap {
      case p: Position => p.entries.map(_.timestamp)
      case t: TopUp    => List(t.timestamp)
      case t: Withdraw => List(t.timestamp)
    }.sorted
    timestamps match {
      case head :: Nil  => Some(TimeInterval(head))
      case head :: tail => Some(TimeInterval(head, tail.last))
      case Nil          => None
    }
  }
}

object MarketPlayService {
  def getPlays(
    userWallet: Wallet
  )(filter: PlayFilter): ZIO[Has[MarketPlayService], MarketPlayError, MarketPlays] =
    ZIO.serviceWith[MarketPlayService](_.getPlays(userWallet, filter))
}

final case class LiveMarketPlayService(
  positionRepo: MarketPlayRepo,
  priceQuoteRepo: PriceQuoteRepo,
  blockchainRepo: BlockchainRepo,
  journalingRepo: JournalingRepo,
  currencyRepo: CurrencyRepo,
  logger: Logger[String]
) extends MarketPlayService {

  override def getPlays(userWallet: Wallet): IO[MarketPlayError, MarketPlays] =
    (for {
      marketPlays   <- positionRepo.getPlays(userWallet.address)
      quotes        <- getQuotesForMarketPlays(marketPlays)
      enrichedPlays = marketPlays.map(play => addQuotes(play, quotes))
    } yield MarketPlays(enrichedPlays)).orElseFail(MarketPlaysFetchError(s"Plays fetch error ${userWallet.address}"))

  override def getPlays(userWallet: Wallet, playFilter: PlayFilter): IO[MarketPlayError, MarketPlays] =
    (for {
      marketPlays    <- positionRepo.getPlays(userWallet.address)
      quotes         <- getQuotesForMarketPlays(marketPlays)
      enrichedPlays  = marketPlays.map(play => addQuotes(play, quotes))
      journalEntries <- journalingRepo.getEntries(userWallet.userId, marketPlays.map(_.id).values)
    } yield MarketPlays(withJournalEntries(enrichedPlays, journalEntries)))
      .orElseFail(MarketPlaysFetchError(s"Plays fetch error ${userWallet.address}"))

  //TODO I don't think I need to return Page from the repo, I could just return the plays.
  override def getPlays(
    userWallet: Wallet,
    filter: PlayFilter,
    contextId: ContextId
  ): IO[MarketPlayError, MarketPlays] = ???
//    for {
//      page        <- positionRepo.getPlays(userWallet.address, filter, contextId)
//      quotes      <- getQuotesForMarketPlays(page.data)
//      enrichedPlays = marketPlays.map(play => addQuotes(play, quotes))
//      journalEntries <- journalingRepo.getEntries(
//                         userWallet.userId,
//                         marketPlays.map(_.id).values
//                       )
//    } yield MarketPlays(withJournalEntries(enrichedPlays, journalEntries).mostRecentFirst())

  private def withJournalEntries(plays: List[MarketPlay], entries: List[JournalEntry]): List[MarketPlay] = {
    val positionToEntryMap = entries.map(e => e.positionId.get -> e).toMap
    plays.map {
      case p: Position                  => p.copy(journal = p.id.flatMap(positionToEntryMap.get))
      case t @ (_: TopUp | _: Withdraw) => t
    }
  }

  private def addQuotes(marketPlay: MarketPlay, currencyQuotes: Map[Currency, List[PriceQuote]]): MarketPlay =
    marketPlay match {
      case p: Position =>
        val interval = p.timeInterval
        val currency = p.currency
        var source = List(
          WBNB -> currencyQuotes.getOrElse(WBNB, List.empty).filter(quote => interval.contains(quote.timestamp))
        )
        if (currency.isDefined) {
          source = source :+ currency.get -> currencyQuotes
            .getOrElse(currency.get, List.empty)
            .filter(quote => interval.contains(quote.timestamp))
        }
        val quotes = PriceQuotes(source.toMap)
        if (quotes.nonEmpty()) {
          p.copy(priceQuotes = Some(quotes))
        } else {
          p
        }
      case t: TopUp =>
        val interval = TimeInterval(t.timestamp.atBeginningOfDay(), t.timestamp.atEndOfDay())
        val quotes = PriceQuotes(
          Map(WBNB -> currencyQuotes.getOrElse(WBNB, List.empty).filter(quote => interval.contains(quote.timestamp)))
        )
        if (quotes.nonEmpty()) {
          t.copy(priceQuotes = Some(quotes))
        } else {
          t
        }
      case w: Withdraw =>
        val interval = TimeInterval(w.timestamp.atBeginningOfDay(), w.timestamp.atEndOfDay())
        val quotes = PriceQuotes(
          Map(WBNB -> currencyQuotes.getOrElse(WBNB, List.empty).filter(quote => interval.contains(quote.timestamp)))
        )
        if (quotes.nonEmpty()) {
          w.copy(priceQuotes = Some(quotes))
        } else {
          w
        }
    }

  override def getPosition(userId: UserId, positionId: PlayId): IO[MarketPlayError, PositionDetails[Position]] =
    //TODO Better error handling with zipPar -> for example if first effect fails with PositionNotFound then API fails silently
    // We lose the error type here.
    for {
      positionWithLinkIds <- positionRepo.getPosition(positionId)
      linkedPositions     <- positionRepo.getPositions(positionWithLinkIds.links.next ++ positionWithLinkIds.links.previous)
      (next, previous)    = linkedPositions.partition(p => positionWithLinkIds.links.next.contains(p.id.get))
      quotes              <- getQuotesForMarketPlays(next ++ previous)
      (nextEnriched, previousEnriched) = (
        next.map(n => addQuotes(n, quotes)).asInstanceOf[List[Position]],
        previous.map(p => addQuotes(p, quotes)).asInstanceOf[List[Position]]
      )
      position = enrichPosition(positionWithLinkIds.position, quotes)
      journalEntry <- journalingRepo.getEntry(userId, position.id.get).map(Some(_)).catchSome {
                       case _: JournalNotFound => UIO.none
                     }
    } yield PositionDetails(position.copy(journal = journalEntry), PositionLinks(nextEnriched, previousEnriched))

  private def enrichPosition(position: Position, currencyQuotes: Map[Currency, List[PriceQuote]]): Position = {
    val interval = position.timeInterval
    val currency = position.currency
    var source = List(
      WBNB -> currencyQuotes.getOrElse(WBNB, List.empty).filter(quote => interval.contains(quote.timestamp))
    )
    if (currency.isDefined) {
      source = source :+ currency.get -> currencyQuotes
        .getOrElse(currency.get, List.empty)
        .filter(quote => interval.contains(quote.timestamp))
    }
    val quotes = PriceQuotes(source.toMap)
    if (quotes.nonEmpty()) {
      position.copy(priceQuotes = Some(quotes))
    } else {
      position
    }
  }

  override def getNextPositions(playId: PlayId): IO[MarketPlayError, List[Position]] =
    for {
      _             <- logger.info(s"Fetch next positions for position ${playId.value}")
      nextIds       <- positionRepo.getNextPositionIds(playId)
      positions     <- positionRepo.getPositions(nextIds)
      quotes        <- getQuotesForMarketPlays(positions)
      enrichedPlays = positions.map(play => addQuotes(play, quotes)).asInstanceOf[List[Position]]
    } yield enrichedPlays

  override def getPreviousPositions(playId: PlayId): IO[MarketPlayError, List[Position]] =
    for {
      _             <- logger.info(s"Fetch previous positions for position ${playId.value}")
      nextIds       <- positionRepo.getPreviousPositionIds(playId)
      positions     <- positionRepo.getPositions(nextIds)
      quotes        <- getQuotesForMarketPlays(positions)
      enrichedPlays = positions.map(play => addQuotes(play, quotes)).asInstanceOf[List[Position]]
    } yield enrichedPlays

  private def getQuotesForMarketPlays(
    marketPlays: List[MarketPlay]
  ): IO[MarketPlayError, Map[Currency, List[PriceQuote]]] = {
    val timeInterval = extractTimeInterval(marketPlays)
    if (timeInterval.isDefined) {
      priceQuoteRepo
        .getQuotes(Set.empty, timeInterval.get)
        .orElseFail(MarketPlaysFetchError(s"Price quotes fetch error"))
    } else {
      UIO(Map.empty[Currency, List[PriceQuote]])
    }
  }

  override def importPlays(userWallet: Wallet): IO[MarketPlayError, Unit] =
    logger.info(s"Importing positions for ${userWallet.address}") *>
      importPlays(userWallet.address, blockchainRepo.transactionsStream(userWallet.address))(userWallet)

  override def importPlays(userWallet: Wallet, startFrom: Instant): IO[MarketPlayError, Unit] =
    importPlays(userWallet.address, blockchainRepo.transactionsStream(userWallet.address, startFrom))(userWallet)

  private def importPlays(
    walletAddress: WalletAddress,
    txStream: ZStream[Any, TransactionsGetError, Transaction]
  )(userWallet: Wallet): IO[MarketPlayError, Unit] = {
    val noPlaysEffect = logger.info(s"No positions to import for ${userWallet.address}")

    @inline
    def handlePlayImport(plays: MarketPlays): IO[MarketPlayError, Unit] =
      for {
        //Get open positions that might become closed with the new data coming in
        openPositions <- positionRepo.getPositions(userWallet.address, Open).map(MarketPlays(_))
        merged        = openPositions.merge(plays)
        _ <- positionRepo
              .save(userWallet.address, merged.plays)
              .mapError(throwable => MarketPlayImportError(userWallet.address, throwable))
        _ <- logger.info(s"Data import complete for ${userWallet.address.value}")
      } yield ()

    for {
      _ <- logger.info(s"Importing data for ${userWallet.address.value}...")

      plays <- txStream.runCollect
                .mapBoth(
                  error => MarketPlayImportError(userWallet.address, new RuntimeException(error.message)),
                  chunks => findMarketPlays(walletAddress, chunks.toList)
                ) // TODO Try to optimize so as not to process the entire stream.

      _ <- if (plays.isEmpty) {
            noPlaysEffect
          } else {
            handlePlayImport(plays)
              .zipRight(
                currencyRepo
                  .upsert(plays.currencies)
                  .orElseFail(MarketPlayImportError(walletAddress, new RuntimeException("Currency upsert failed")))
              )
          }
    } yield ()
  }
}

object LiveMarketPlayService {
  lazy val layer: URLayer[Has[MarketPlayRepo] with Has[PriceQuoteRepo] with Has[BlockchainRepo] with Has[
    JournalingRepo
  ] with Has[CurrencyRepo] with Logging, Has[
    MarketPlayService
  ]] =
    (LiveMarketPlayService(_, _, _, _, _, _)).toLayer
}
