module MyTrades.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import CommonTypes exposing (UserInfo)
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


init : EthHelpers.EthNode -> UserInfo -> ( Model, Cmd Msg )
init ethNode userInfo =
    let
        ( tradeCache, tcCmd ) =
            TradeCache.initAndStartCaching ethNode
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , tradeCache = tradeCache
      }
    , tcCmd |> Cmd.map TradeCacheMsg
    )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Routing.Route )
update msg model =
    case msg of
        TradeClicked id ->
            ( model, Cmd.none, Just (Routing.Trade id) )

        TradeCacheMsg tradeCacheMsg ->
            let
                ( newTradeCache, tcCmd ) =
                    TradeCache.update
                        tradeCacheMsg
                        model.tradeCache
            in
            ( { model | tradeCache = newTradeCache }
            , tcCmd |> Cmd.map TradeCacheMsg
            , Nothing
            )

        NoOp ->
            noUpdate model


noUpdate : Model -> ( Model, Cmd Msg, Maybe Routing.Route )
noUpdate model =
    ( model, Cmd.none, Nothing )


updateUserInfo : UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
