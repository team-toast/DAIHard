module TradeCache.Types exposing (DataState, LoadingStatus(..), Msg(..), TradeCache, UpdateResult, justModelUpdate, loadingStatus)

import AppCmd exposing (AppCmd)
import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import Time
import TokenValue exposing (TokenValue)


type alias TradeCache =
    { factory : FactoryType
    , eventSentry : EventSentry Msg
    , trades : Array CTypes.Trade
    , dataFetchState : DataState
    }


type alias DataState =
    { total : Maybe Int
    , loaded : Int
    , invalid : Int
    }


type LoadingStatus
    = QueryingNumTrades
    | NoneFound
    | FetchingTrades
    | AllFetched


type Msg
    = InitialNumTradesFetched (Result Http.Error BigInt)
    | CheckForNewTrades
    | UpdateTradePhases
    | PhaseFetched FactoryType Int (Result Http.Error (Maybe CTypes.Phase))
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


loadingStatus : TradeCache -> LoadingStatus
loadingStatus tc =
    case tc.dataFetchState.total of
        Nothing ->
            QueryingNumTrades

        Just 0 ->
            NoneFound

        Just totalTrades ->
            if tc.dataFetchState.loaded < (totalTrades - tc.dataFetchState.invalid) then
                FetchingTrades

            else
                AllFetched


justModelUpdate : TradeCache -> UpdateResult
justModelUpdate tc =
    UpdateResult
        tc
        Cmd.none
        []
