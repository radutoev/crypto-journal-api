package io.softwarechain.cryptojournal
package domain.position

import domain.model._
import domain.model.fungible.{FungibleDataMapOps, FungibleDataOps, OptionalFungibleDataOps}
import domain.position.error.InvalidPosition
import domain.position.model.CoinName
import domain.pricequote.PriceQuote
import util.ListOptionOps
import vo.TimeInterval

import java.time.{Duration, Instant}

final case class Position(
  entries: List[PositionEntry],
  dataSource: Option[PositionData] = None,
  journal: Option[JournalEntry] = None,
  id: Option[PlayId] = None
) extends MarketPlay {
  lazy val timeInterval: TimeInterval = closedAt.fold(TimeInterval(openedAt))(closed => TimeInterval(openedAt, closed))

  /**
   * Number of transactions that are part of this position.
   */
  lazy val numberOfExecutions: Int = entries.map(_.hash).distinct.size

  lazy val currency: Option[Currency] = dataSource.flatMap(_.currency(entries))

  //TODO I think I need to see if I can add the coin address to all transaction types.
  lazy val coinAddress: Option[CoinAddress] = {
    entries.map {
      case _: AirDrop                                   => None
      case _: Approval                                  => None
      case Buy(_, _, _, _, coinAddress, _, _, _, _, _)  => Some(coinAddress)
      case Claim(_, _, _, _, _, _, _, _)                => None
      case _: Contribute                                => None
      case Sell(_, _, _, _, _, _)                       => None
      case TransferIn(_, _, _, _, _, _, _, coinAddress) => coinAddress
      case TransferOut(_, _, _, _, _, _)                => None
    }.collectFirst {
      case Some(address) => address
    }
  }

  lazy val coinName: Option[CoinName] = entries.map {
    case AirDrop(name, _, _, _, _, _, _, _)    => Some(name)
    case _: Approval                           => None
    case Buy(_, _, _, _, name, _, _, _, _, _)  => Some(name)
    case Claim(_, _, _, name, _, _, _, _)      => Some(name)
    case _: Contribute                         => None
    case _: Sell                               => None
    case TransferIn(_, _, _, _, _, name, _, _) => name
    case _: TransferOut                        => None
  }.collectFirst {
    case Some(coinName) => coinName
  }

  lazy val cost: Map[Currency, FungibleData] = dataSource.map(_.cost(entries)).getOrElse(Map.empty)

  lazy val fees: Map[Currency, FungibleData] = dataSource.map(_.fees(entries)).getOrElse(Map.empty)

  lazy val totalCost: Map[Currency, FungibleData] = {
    List(cost, fees).sumByCurrency
  }

  /**
   * Position return derived from all position entries associated with this position.
   *
   * @return
   * None if position is not closed or no price quotes are given for the position interval.
   * FungibleData for a closed position.
   */
  lazy val fiatReturn: Option[FungibleData] = {
    if (state == Closed) {
      totalCost.get(USD).map(cost => fiatSellValue.subtract(cost.amount))
    } else {
      None
    }
  }

  /**
   * Percentage difference calculated as:
   * ((totalCost - fiatReturn) / totalCost) * 100.
   */
  lazy val fiatReturnPercentage: Option[BigDecimal] =
    if (state == Open) {
      None
    } else {
      for {
        totalCost  <- totalCost.get(USD)
        fiatReturn <- fiatReturn
      } yield util.math.percentageDiff(totalCost.amount, fiatReturn.amount)
    }

  /**
   * @return Total number of coins that were bought within this position
   */
  lazy val totalCoins: FungibleData = {
    lazy val coinsByCurrency = entries.map {
      case AirDrop(_, _, _, received, _, _, _, _)   => Some(received)
      case _: Approval                              => None
      case Buy(_, _, received, _, _, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _, _, _)     => Some(received)
      case _: Contribute                            => None
      case _: Sell                                  => None
      case t: TransferIn                            => Some(t.value)
      case _: TransferOut                           => None
    }.values.sumByCurrency

    currency.flatMap(coinsByCurrency.get).getOrElse(FungibleData.zero(WBNB))
  }

  lazy val orderSize: BigDecimal = totalCoins.amount

  /**
   * @return Order size if only one buy or order size divided by number of buys if multiple buys in position.
   */
  lazy val averageOrderSize: BigDecimal = {
    val nrOfBuys = entries.count {
      case _: TransferOut | _: Sell => false
      case _                        => true
    }
    if (nrOfBuys > 0) {
      orderSize / nrOfBuys
    } else {
      BigDecimal(0)
    }
  }

  /**
   * @return Entry coin fiat price
   */
  lazy val entryPrice: Option[PriceQuote] = dataSource.flatMap(_.entryPrice(entries))

  /**
   * @return Exit coin fiat price
   */
  lazy val exitPrice: Option[PriceQuote] = dataSource.flatMap(_.exitPrice(entries))

  /**
   * Number of coins that are part of this Position
   */
  lazy val numberOfCoins: BigDecimal = {
    lazy val currentCoins = entries.map {
      case AirDrop(_, _, _, received, _, _, _, _)   => Some(received)
      case _: Approval                              => None
      case Buy(_, _, received, _, _, _, _, _, _, _) => Some(received)
      case Claim(_, received, _, _, _, _, _, _)     => Some(received)
      case _: Contribute                            => None
      case Sell(sold, _, _, _, _, _)                => Some(sold.negate())
      case t: TransferIn                            => Some(t.value)
      case TransferOut(amount, _, _, _, _, _)       => Some(amount.negate())
    }

    lazy val coinsByCurrency = currentCoins.sumByCurrency

    currency
      .map(value => coinsByCurrency.getOrElse(value, FungibleData.zero(value)))
      .map(_.amount)
      .getOrElse(BigDecimal(0))
  }

  lazy val isWin: Option[Boolean] = fiatReturn.map(_.amount.compareTo(BigDecimal(0)) > 0)

  lazy val isLoss: Option[Boolean] = isWin.map(b => !b)

  lazy val state: State =
    entries.lastOption.fold[State](Open) {
      case _: Sell => Closed
      case _       => Open
    }

  lazy val isClosed: Boolean = state == Closed

  lazy val isOpen: Boolean = state == Open

  lazy val openedAt: Instant = entries.head.timestamp

  lazy val closedAt: Option[Instant] = dataSource.flatMap(_.closedAt(entries))

  lazy val holdTime: Option[Long] = closedAt.map(closeTime => Duration.between(openedAt, closeTime).toSeconds)

  lazy val fiatSellValue: FungibleData = dataSource.map(_.fiatSellValue(entries)).getOrElse(FungibleData.zero(USD))

  override def inInterval(interval: TimeInterval): Boolean = {
    val moreRecentThanStart = interval.start.isBefore(openedAt) || interval.start == openedAt
    closedAt.fold(moreRecentThanStart)(t => moreRecentThanStart && (interval.end.isAfter(t) || interval.end == t))
  }

  //hardcoded to USD for now
  lazy val balance: Option[FungibleData] = dataSource.flatMap(_.balance(entries))
}

object Position {
  def apply(entries: List[PositionEntry]): Either[InvalidPosition, Position] =
    if (isSorted(entries.map(_.timestamp))(Ordering[Instant])) {
      Right(new Position(entries))
    } else {
      Left(InvalidPosition("Entries not in chronological order"))
    }

  def apply(entries: List[PositionEntry], id: PlayId): Position = new Position(entries, id = Some(id))

  def unsafeApply(entries: List[PositionEntry]): Position = new Position(entries)

  def isSorted[T](seq: Seq[T])(implicit ord: Ordering[T]): Boolean = seq match {
    case Seq()  => true
    case Seq(_) => true
    case _      => seq.sliding(2).forall { case Seq(first, second) => ord.lteq(first, second) }
  }
}
