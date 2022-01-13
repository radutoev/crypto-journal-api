package io.softwarechain.cryptojournal
package infrastructure.api

import application.CryptoJournalApi
import domain.model._
import domain.position
import domain.position.error._
import domain.position.model.ScamStrategy
import domain.position.{JournalingService, MarketPlayService, MarketPlays, JournalEntry => CJJournalEntry, MarketPlay => CJMarketPlay, Position => CJPosition, PositionDetails => CJPositionDetails, PositionEntry => CJPositionEntry, PositionJournalEntry => CJPositionJournalEntry, TopUp => CJTopUp, Withdraw => CJWithdraw}
import domain.pricequote.{PriceQuote => CJPriceQuote}
import infrastructure.api.Routes.ApiError
import infrastructure.api.common.dto.FungibleData
import infrastructure.api.common.dto._
import infrastructure.api.common.{CountQParamOps, IntervalQParamsOps, QParamsOps}
import infrastructure.api.plays.dto.{JournalEntry, PositionJournalEntry, fromMarketPlay, fromPosition, fromPositionDetails, positionDetailsCodec}
import infrastructure.auth.JwtRequestContext
import util.ListEitherOps
import vo.filter.PlayFilter

import eu.timepit.refined.refineV
import zhttp.http.HttpError.BadRequest
import zhttp.http._
import zio.json._
import zio.prelude.Validation
import zio.{Chunk, Has, ZIO}

import java.time.Instant

object plays {
  def routes(userId: UserId, contextId: ContextId) = HttpApp.collectM {
    case req @ Method.GET -> Root / "addresses" / rawWalletAddress / "latest-plays" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        filter <- req.url.playsFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getLatestPlays(address, filter)
                     .provideSomeLayer[Has[MarketPlayService]](JwtRequestContext.layer(userId, contextId))
                     .fold(marketPlayErrorToHttpResponse, _.asResponse(contextId))
      } yield response

    case req @ Method.GET -> Root / "addresses" / rawWalletAddress / "plays" =>
      for {
        address <- ZIO
                    .fromEither(refineV[WalletAddressPredicate](rawWalletAddress))
                    .orElseFail(BadRequest("Invalid address"))

        filter <- req.url.playsFilter().toZIO.mapError(reason => BadRequest(reason))

        response <- CryptoJournalApi
                     .getPlays(address, filter)
                     .provideSomeLayer[Has[MarketPlayService]](JwtRequestContext.layer(userId, contextId))
                     .fold(marketPlayErrorToHttpResponse, _.asResponse(contextId))
      } yield response

    case Method.GET -> Root / "positions" / rawPositionId =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PlayIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        response <- CryptoJournalApi
                     .getPosition(positionId)
                     .provideSomeLayer[Has[MarketPlayService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       marketPlayErrorToHttpResponse,
                       positionDetails => {
                         Response.jsonString(fromPositionDetails(positionDetails).toJson(positionDetailsCodec))
                       }
                     )
      } yield response

    case Method.GET -> Root / "positions" / rawPositionId / "previous" =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PlayIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        response <- CryptoJournalApi
                     .getPreviousPositions(positionId)
                     .provideSomeLayer[Has[MarketPlayService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       marketPlayErrorToHttpResponse,
                       positions => Response.jsonString(positions.map(fromPosition).toJson)
                     )
      } yield response

    case Method.GET -> Root / "positions" / rawPositionId / "next" =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PlayIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        response <- CryptoJournalApi
                     .getNextPositions(positionId)
                     .provideSomeLayer[Has[MarketPlayService]](JwtRequestContext.layer(userId, contextId))
                     .fold(
                       marketPlayErrorToHttpResponse,
                       positions => Response.jsonString(positions.map(fromPosition).toJson)
                     )
      } yield response

    case req @ Method.PUT -> Root / "positions" / rawPositionId / "journal" =>
      for {
        positionId <- ZIO
                       .fromEither(refineV[PlayIdPredicate](rawPositionId))
                       .orElseFail(BadRequest("Invalid positionId"))

        journalEntry <- ZIO
                         .fromOption(req.getBodyAsString)
                         .flatMap(rawBody => ZIO.fromEither(rawBody.fromJson[JournalEntry]))
                         .orElseFail(BadRequest("Invalid request"))

        response <- CryptoJournalApi
                     .saveJournalEntry(positionId, journalEntry.toDomainModel)
                     .provideSomeLayer[Has[JournalingService]](JwtRequestContext.layer(userId, contextId))
                     .fold(marketPlayErrorToHttpResponse, _ => Response.status(Status.OK))
      } yield response

    case req @ Method.PUT -> Root / "journal" =>
      for {
        entries <- ZIO
                    .fromOption(req.getBodyAsString)
                    .flatMap(rawBody =>
                      ZIO.fromEither(rawBody.fromJson[List[PositionJournalEntry]]).map(_.map(_.toDomainModel))
                    )
                    .orElseFail(BadRequest("Invalid request"))
        response <- CryptoJournalApi
                     .saveJournalEntries(entries)
                     .provideSomeLayer[Has[JournalingService]](JwtRequestContext.layer(userId, contextId))
                     .fold(marketPlayErrorToHttpResponse, _ => Response.status(Status.OK))
      } yield response
  }

  implicit class PlaysQParamsOps(url: URL) extends QParamsOps {
    def playsFilter(): Validation[String, PlayFilter] = {
      val qParams = url.queryParams.map { case (key, values) => key.toLowerCase -> values.head }

      Validation.validateWith(
        url.countFilter(),
        url.intervalFilter()
      ) {
        case (count, interval) =>
          new PlayFilter(count, interval)
      }
    }
  }

  implicit class PositionsResponseOps(marketPlays: MarketPlays) {
    def asResponse(contextId: ContextId): UResponse = {
      val headers =
        Header("Content-Type", "application/json") :: Header("X-CoinLogger-ContextId", contextId.value) :: Nil

      marketPlays.plays match {
        case Nil => Response.http(status = Status.NO_CONTENT, headers = headers)
        case list =>
          Response.http(
            status = Status.OK,
            headers = headers,
            content = HttpData.CompleteData(
              Chunk.fromArray(list.map(fromMarketPlay).toJson.getBytes(HTTP_CHARSET))
            )
          )
      }
    }
  }

  val marketPlayErrorToHttpResponse: MarketPlayError => UResponse = {
    case InvalidRepresentation(message) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "InvalidRepresentation", message).toResponsePayload
      )
    case MarketPlaysFetchError(address) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content =
          ApiError(`type` = "PositionsFetchError", s"Error retrieving positions for address: $address").toResponsePayload
      )
    case MarketPlayNotFound(_) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionNotFound").toResponsePayload
      )
    case MarketPlayFetchError(positionId, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionFetchError", s"Error retrieving position $positionId").toResponsePayload
      )
    case PriceQuotesError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content =
          ApiError(`type` = "PriceQuotesError", s"Failure applying price quotes onp positions").toResponsePayload
      )
    case CheckpointFetchError(address, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "CheckpointFetchError").toResponsePayload
      )
    case CheckpointNotFound(address) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = s"No checkpoint for address $address").toResponsePayload
      )
    case JournalSaveError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalSaveError").toResponsePayload
      )
    case JournalFetchError(throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalFetchError").toResponsePayload
      )
    case JournalNotFound(userId, positionId) =>
      Response.http(
        status = Status.NOT_FOUND,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "JournalNotFound").toResponsePayload
      )
    case MarketPlayImportError(address, throwable) =>
      Response.http(
        status = Status.INTERNAL_SERVER_ERROR,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "PositionImportError").toResponsePayload
      )
    case InvalidInput(reason) =>
      Response.http(
        status = Status.BAD_REQUEST,
        headers = List(Header("Content-Type", "application/json")),
        content = ApiError(`type` = "InvalidInput", reason).toResponsePayload
      )
  }

  object dto {
    sealed trait MarketPlay

    final case class TopUp(
      hash: String,
      value: FungibleData,
      fees: Map[String, FungibleData],
      timestamp: Instant,
      id: Option[String]
    ) extends MarketPlay

    final case class Withdrawal(
      hash: String,
      value: FungibleData,
      fees: Map[String, FungibleData],
      timestamp: Instant,
      id: Option[String]
    ) extends MarketPlay

    final case class Position(
      currency: String,
      address: Option[String],
      state: String,
      openedAt: Instant,
      closedAt: Option[Instant],
      costs: Map[String, FungibleData],
      fees: Map[String, FungibleData],
      fiatReturn: Option[FungibleData],
      returnPercentage: Option[BigDecimal],
      totalCoins: FungibleData,
      orderSize: BigDecimal,
      averageOrderSize: BigDecimal,
      entryPrice: Option[PriceQuote],
      exitPrice: Option[PriceQuote],
      numberOfExecutions: Int,
      holdTime: Option[Long],
      win: Option[Boolean],
      entries: List[PositionEntry],
      id: Option[String],
      journalEntry: JournalEntry
    ) extends MarketPlay

    sealed trait PositionEntry
    final case class AirDrop(
      name: String,
      receivedFrom: String,
      fee: FungibleData,
      received: FungibleData,
      coinAddress: String,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry
    final case class Approval(
      fee: FungibleData,
      forContract: String,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry
    final case class Buy(
      fee: FungibleData,
      spent: FungibleData,
      received: FungibleData,
      receivedFrom: String,
      name: String,
      coinAddress: String,
      hash: String,
      timestamp: Instant,
      spentOriginal: Option[FungibleData],
      id: Option[String]
    ) extends PositionEntry
    final case class Claim(
      fee: FungibleData,
      received: FungibleData,
      receivedFrom: String,
      name: String,
      coinAddress: String,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry
    final case class Contribute(
      spent: FungibleData,
      to: String,
      fee: FungibleData,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry
    final case class Sell(
      sold: FungibleData,
      received: FungibleData,
      fee: FungibleData,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry
    final case class TransferIn(
      value: FungibleData,
      receivedFrom: String,
      fee: FungibleData,
      hash: String,
      timestamp: Instant,
      name: Option[String],
      coinAddress: Option[String],
      id: Option[String]
    ) extends PositionEntry
    final case class TransferOut(
      amount: FungibleData,
      to: String,
      fee: FungibleData,
      hash: String,
      timestamp: Instant,
      id: Option[String]
    ) extends PositionEntry

    final case class PositionDetails(position: Position, links: PositionLinks)

    final case class PositionLinks(previous: List[Position], next: List[Position])

    final case class PriceQuote(price: Double, timestamp: Instant)

    final case class PositionJournalEntry(positionId: String, entry: JournalEntry)

    final case class JournalEntry(
      notes: Option[String],
      tags: List[String],
      mistakes: List[String],
      scamStrategy: Option[String]
    )

    implicit val journalEntryCodec: JsonCodec[JournalEntry]         = DeriveJsonCodec.gen[JournalEntry]
    implicit val priceQuoteCodec: JsonCodec[PriceQuote]             = DeriveJsonCodec.gen[PriceQuote]
    implicit val positionEntryCodec: JsonCodec[PositionEntry]       = DeriveJsonCodec.gen[PositionEntry]
    implicit val positionCodec: JsonCodec[Position]                 = DeriveJsonCodec.gen[Position]
    implicit val transferInPlayCodec: JsonCodec[TopUp]              = DeriveJsonCodec.gen[TopUp]
    implicit val marketPlayCodec: JsonCodec[MarketPlay]             = DeriveJsonCodec.gen[MarketPlay]
    implicit val positionTagsCodec: JsonCodec[PositionJournalEntry] = DeriveJsonCodec.gen[PositionJournalEntry]
    implicit val positionLinksCodec: JsonCodec[PositionLinks]       = DeriveJsonCodec.gen[PositionLinks]
    implicit val positionDetailsCodec: JsonCodec[PositionDetails]   = DeriveJsonCodec.gen[PositionDetails]

    def fromPositionDetails(pd: CJPositionDetails[CJPosition]): PositionDetails =
      PositionDetails(
        position = fromPosition(pd.position),
        links = PositionLinks(
          previous = pd.links.previous.map(fromPosition),
          next = pd.links.next.map(fromPosition)
        )
      )

    def fromPriceQuote(q: CJPriceQuote): PriceQuote =
      PriceQuote(q.price, q.timestamp)

    def fromMarketPlay(m: CJMarketPlay): MarketPlay =
      m match {
        case pos: CJPosition  => fromPosition(pos)
        case t: CJTopUp       => fromTopUp(t)
        case tOut: CJWithdraw => fromWithdrawal(tOut)
      }

    def fromPosition(position: CJPosition): Position =
      Position(
        position.currency.map(_.value).getOrElse(""),
        position.coinAddress.map(_.value),
        position.state.toString,
        position.openedAt,
        position.closedAt,
        position.cost.map { case (currency, data) => currency.value -> data.asJson },
        position.fees.map { case (currency, data) => currency.value -> data.asJson },
        position.fiatReturn.asJson,
        position.fiatReturnPercentage,
        position.totalCoins.asJson,
        position.orderSize,
        position.averageOrderSize,
        position.entryPrice.asJson,
        position.exitPrice.asJson,
        position.numberOfExecutions,
        position.holdTime,
        position.isWin.map(isWin => if (isWin) true else false),
        position.entries.map(entry => fromPositionEntry(entry)),
        position.id.map(_.value),
        position.journal.map(_.toDto).getOrElse(JournalEntry(None, List.empty, List.empty, None))
      )

    def fromPositionEntry(entry: CJPositionEntry): PositionEntry =
      entry match {
        case position.AirDrop(name, receivedFrom, fee, received, coinAddress, hash, timestamp, id) =>
          AirDrop(
            name.value,
            receivedFrom.value,
            FungibleData(fee),
            FungibleData(received),
            coinAddress.value,
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Approval(fee, forContract, hash, timestamp, id) =>
          Approval(
            FungibleData(fee),
            forContract.value,
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Buy(fee, spent, received, receivedFrom, name, coinAddress, hash, timestamp, spentOriginal, id) =>
          Buy(
            FungibleData(fee),
            FungibleData(spent),
            FungibleData(received),
            receivedFrom.value,
            name.value,
            coinAddress.value,
            hash.value,
            timestamp,
            spentOriginal.map(f => FungibleData(f)),
            id.map(_.value)
          )
        case position.Claim(fee, received, receivedFrom, name, coinAddress, hash, timestamp, id) =>
          Claim(
            FungibleData(fee),
            FungibleData(received),
            receivedFrom.value,
            name.value,
            coinAddress.value,
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Contribute(spent, to, fee, hash, timestamp, id) =>
          Contribute(
            FungibleData(spent),
            to.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.Sell(sold, received, fee, hash, timestamp, id) =>
          Sell(
            FungibleData(sold),
            FungibleData(received),
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
        case position.TransferIn(value, receivedFrom, fee, hash, timestamp, id, name, coinAddress) =>
          TransferIn(
            FungibleData(value),
            receivedFrom.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            name.map(_.value),
            coinAddress.map(_.value),
            id.map(_.value)
          )
        case position.TransferOut(amount, to, fee, hash, timestamp, id) =>
          TransferOut(
            FungibleData(amount),
            to.value,
            FungibleData(fee),
            hash.value,
            timestamp,
            id.map(_.value)
          )
      }

    private def fromTopUp(t: CJTopUp): TopUp =
      TopUp(
        hash = t.txHash.value,
        value = t.value.asJson,
        fees = t.fees.map { case (currency, fee) => currency.value -> fee.asJson },
        timestamp = t.timestamp,
        id = t.id.map(_.value)
      )

    private def fromWithdrawal(t: CJWithdraw): Withdrawal =
      Withdrawal(
        hash = t.txHash.value,
        value = t.value.asJson,
        fees = t.fees.map { case (currency, fee) => currency.value -> fee.asJson },
        timestamp = t.timestamp,
        id = t.id.map(_.value)
      )

    implicit class PositionTagsOps(posJournalEntry: PositionJournalEntry) {
      //TODO Add validation.
      def toDomainModel: CJPositionJournalEntry =
        CJPositionJournalEntry(
          positionId = refineV[PlayIdPredicate].unsafeFrom(posJournalEntry.positionId),
          entry = posJournalEntry.entry.toDomainModel
        )
    }

    implicit class PriceQuoteOps(data: CJPriceQuote) {
      def asJson: PriceQuote = PriceQuote(data.price, data.timestamp)
    }

    implicit class OptionPriceQuoteOps(data: Option[CJPriceQuote]) {
      def asJson: Option[PriceQuote] = data.map(_.asJson)
    }

    implicit class JournalEntryOps(entry: JournalEntry) {
      def toDomainModel: CJJournalEntry =
        CJJournalEntry(
          entry.notes,
          tags = entry.tags.map(refineV[TagPredicate](_)).rights,
          mistakes = entry.mistakes.map(refineV[MistakePredicate](_)).rights,
          scamStrategy = entry.scamStrategy.flatMap(ScamStrategy(_).toOption)
        )
    }

    implicit class DomainJournalEntryOps(entry: CJJournalEntry) {
      def toDto: JournalEntry =
        JournalEntry(
          entry.notes,
          entry.tags.map(_.value),
          entry.mistakes.map(_.value),
          entry.scamStrategy.map(_.toString)
        )
    }
  }
}
