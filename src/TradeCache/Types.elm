module TradeCache.Types exposing (Msg(..), Status, TradeCache, UpdateResult, justModelUpdate)

import AppCmd exposing (AppCmd)
import Array exposing (Array)
import BigInt exposing (BigInt)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Http
import Json.Decode
import Time
import TokenValue exposing (TokenValue)


type alias TradeCache =
    { web3Context : Web3Context
    , eventSentry : EventSentry Msg
    , trades : Array CTypes.Trade
    , dataFetchStatus : Status
    }


type alias Status =
    { total : Maybe Int
    , loaded : Int
    , invalid : Int
    }


type Msg
    = InitialNumTradesFetched (Result Http.Error BigInt)
    | CheckForNewTrades
    | NumTradesFetchedAgain (Result Http.Error BigInt)
    | CreationInfoFetched Int (Result Http.Error DHF.CreatedTrade)
    | ParametersFetched Int (Result Http.Error (Result String CTypes.TradeParameters))
    | StateFetched Int (Result Http.Error (Maybe CTypes.State))
    | PhaseStartInfoFetched Int (Result Http.Error (Maybe CTypes.PhaseStartInfo))
    | InitiatedEventDataFetched Int (Result Json.Decode.Error DHT.Initiated)
    | EventSentryMsg EventSentry.Msg


type alias UpdateResult =
    { tradeCache : TradeCache
    , cmd : Cmd Msg
    , appCmds : List (AppCmd Msg)
    }


justModelUpdate : TradeCache -> UpdateResult
justModelUpdate tc =
    UpdateResult
        tc
        Cmd.none
        []
