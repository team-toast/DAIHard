module AgentHistory.State exposing (init, subscriptions, update, updateWalletState)

import AgentHistory.Types exposing (..)
import AppCmd
import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Config exposing (..)
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Prices exposing (Price)
import Filters.State as Filters
import Filters.Types as Filter
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.State as TradeTable
import TradeTable.Types as TradeTable
import Wallet


init : Wallet.State -> Address -> ( Model, Cmd Msg )
init wallet agentAddress =
    ( { wallet = wallet
      , agentAddress = agentAddress
      , filters =
            Filters.init
                [ Filter.phases True True True False
                , Filter.offerType True True
                , Filter.role agentAddress True True
                ]
      , tradeTable =
            TradeTable.init
                ( TradeTable.Phase, TradeTable.Ascending )
      }
    , Cmd.none
    )


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        Poke address ->
            let
                txParams =
                    DHT.poke
                        address
                        |> Eth.toSend

                customSend =
                    { onMined = Nothing
                    , onSign = Nothing
                    , onBroadcast = Nothing
                    }

                chainCmd =
                    ChainCmd.custom
                        customSend
                        txParams
            in
            UpdateResult
                prevModel
                Cmd.none
                chainCmd
                []

        TradeClicked factory id ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ AppCmd.GotoRoute (Routing.Trade factory id) ]

        FiltersMsg filtersMsg ->
            justModelUpdate
                { prevModel
                    | filters =
                        prevModel.filters |> Filters.update filtersMsg
                }

        TradeTableMsg tradeTableMsg ->
            let
                ttUpdateResult =
                    prevModel.tradeTable
                        |> TradeTable.update tradeTableMsg
            in
            UpdateResult
                { prevModel
                    | tradeTable = ttUpdateResult.model
                }
                (Cmd.map TradeTableMsg ttUpdateResult.cmd)
                (ChainCmd.map TradeTableMsg ttUpdateResult.chainCmd)
                (List.map (AppCmd.map TradeTableMsg) ttUpdateResult.appCmds)

        NoOp ->
            noUpdate prevModel


noUpdate : Model -> UpdateResult
noUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []


updateWalletState : Wallet.State -> Model -> Model
updateWalletState wallet model =
    { model | wallet = wallet }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
