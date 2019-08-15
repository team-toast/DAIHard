module AgentHistory.Types exposing (Model, Msg(..), UpdateResult, justModelUpdate)

import AppCmd exposing (AppCmd)
import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Filters.Types as Filters
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import Wallet


type alias Model =
    { wallet : Wallet.State
    , agentAddress : Address
    , filters : Filters.Model
    , tradeTable : TradeTable.Model
    }


type Msg
    = Poke Address
    | TradeClicked FactoryType Int
    | FiltersMsg Filters.Msg
    | TradeTableMsg TradeTable.Msg
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , appCmds : List (AppCmd Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []
