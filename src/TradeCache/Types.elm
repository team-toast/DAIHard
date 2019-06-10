module TradeCache.Types exposing (Msg(..), TradeCache)

import Array exposing (Array)
import BigInt exposing (BigInt)
import Contracts.Generated.DAIHardNativeFactory as DHF
import Contracts.Generated.DAIHardNativeTrade as DHT
import Contracts.Types as CTypes
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Helpers.Eth as EthHelpers exposing (EthNode)
import Http
import Json.Decode
import Time
import TokenValue exposing (TokenValue)


type alias TradeCache =
    { ethNode : EthNode
    , eventSentry : EventSentry Msg
    , numTrades : Maybe Int
    , trades : Array CTypes.Trade
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
