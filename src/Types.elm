module Types exposing (Flags, Model, Msg(..), Submodel(..))

import AgentHistory.Types
import Array exposing (Array)
import BigInt exposing (BigInt)
import Browser
import Browser.Navigation
import CmdDown
import CmdUp
import CommonTypes exposing (..)
import Create.Types
import Eth.Net
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Helpers.Eth as EthHelpers
import Json.Decode
import Marketplace.Types
import Routing
import SugarSale.Types
import Time
import Trade.Types
import TradeCache.Types as TradeCache exposing (TradeCache)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)
import Wallet


type alias Flags =
    { networkId : Int
    , width : Int
    , height : Int
    , nowInMillis : Int
    }


type alias Model =
    { key : Browser.Navigation.Key
    , testMode : Bool
    , pageRoute : Routing.PageRoute
    , userAddress : Maybe Address -- `wallet` will store this but only after commPubkey has been generated
    , wallet : Wallet.State
    , now : Time.Posix
    , txSentry : Maybe (TxSentry Msg)
    , tradeCaches : List TradeCache
    , submodel : Submodel
    , userNotices : List (UserNotice Msg)
    , dProfile : DisplayProfile
    }


type Submodel
    = InitialBlank
    | FetchingRedeployForCreate TradeReference
    | CreateModel Create.Types.Model
    | TradeModel Trade.Types.Model
    | MarketplaceModel Marketplace.Types.Model
    | AgentHistoryModel AgentHistory.Types.Model
    | SugarSaleModel SugarSale.Types.Model


type Msg
    = Resize Int Int
    | LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotoRoute Routing.PageRoute
    | CheckIfRedeployFetchComplete
    | Tick Time.Posix
    | CmdUp (CmdUp.CmdUp Msg)
    | ConnectToWeb3
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | UserPubkeySet Json.Decode.Value
    | CreateMsg Create.Types.Msg
    | TradeCacheMsg Int TradeCache.Msg
    | TradeMsg Trade.Types.Msg
    | MarketplaceMsg Marketplace.Types.Msg
    | AgentHistoryMsg AgentHistory.Types.Msg
    | SugarSaleMsg SugarSale.Types.Msg
    | DismissNotice Int
    | ClickHappened
    | NoOp
    | Test String
