module MyTrades.Types exposing (Model, Msg(..))

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.DAIHardFactory as DHF
import Contracts.Generated.DAIHardTrade as DHT
import Contracts.Types as CTypes
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers exposing (EthNode)
import FiatValue exposing (FiatValue)
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)


type alias Model =
    { ethNode : EthNode
    , userInfo : UserInfo
    , tradeCache : TradeCache
    }


type Msg
    = TradeClicked Int
    | TradeCacheMsg TradeCache.Msg
    | NoOp
