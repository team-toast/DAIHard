module AgentHistory.Types exposing (Model, Msg(..), UpdateResult)

import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
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
import Routing
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)


type alias Model =
    { ethNode : EthNode
    , agentAddress : Address
    , agentRole : BuyerOrSeller
    , userInfo : Maybe UserInfo
    , viewPhase : CTypes.Phase
    }


type Msg
    = ViewUserRoleChanged BuyerOrSeller
    | ViewPhaseChanged CTypes.Phase
    | Poke Address
    | TradeClicked Int
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , newRoute : Maybe Routing.Route
    }
