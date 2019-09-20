module AgentHistory.Types exposing (Model, Msg(..), UpdateResult, justModelUpdate)

import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies exposing (Price)
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Filters.Types as Filters
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import PriceFetch
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
    , prices : List ( Currencies.Symbol, PriceFetch.PriceData )
    , now : Time.Posix
    }


type Msg
    = Poke Address
    | TradeClicked FactoryType Int
    | FiltersMsg Filters.Msg
    | TradeTableMsg TradeTable.Msg
    | UpdateNow Time.Posix
    | Refresh
    | PricesFetched (Result Http.Error (List ( Currencies.Symbol, PriceFetch.PriceAndTimestamp )))
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []
