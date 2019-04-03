module MyTrades.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import CommonTypes exposing (..)
import Constants exposing (..)
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import MyTrades.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache


init : EthHelpers.EthNode -> Maybe UserInfo -> ( Model, Cmd Msg )
init ethNode userInfo =
    let
        ( tradeCache, tcCmd ) =
            TradeCache.initAndStartCaching ethNode
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , tradeCache = tradeCache
      , viewUserRole = Seller
      , viewPhase = CTypes.Committed
      }
    , tcCmd |> Cmd.map TradeCacheMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Routing.Route )
update msg prevModel =
    case msg of
        ViewUserRoleChanged role ->
            ( { prevModel | viewUserRole = role }
            , Cmd.none
            , Nothing
            )

        ViewPhaseChanged phase ->
            ( { prevModel | viewPhase = phase }
            , Cmd.none
            , Nothing
            )

        TradeClicked id ->
            ( prevModel, Cmd.none, Just (Routing.Trade id) )

        TradeCacheMsg tradeCacheMsg ->
            let
                ( newTradeCache, tcCmd ) =
                    TradeCache.update
                        tradeCacheMsg
                        prevModel.tradeCache
            in
            ( { prevModel | tradeCache = newTradeCache }
            , tcCmd |> Cmd.map TradeCacheMsg
            , Nothing
            )

        NoOp ->
            noUpdate prevModel


noUpdate : Model -> ( Model, Cmd Msg, Maybe Routing.Route )
noUpdate model =
    ( model, Cmd.none, Nothing )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
