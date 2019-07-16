module Types exposing (Flags, InitialWeb3State(..), Model, Msg(..), Submodel(..))

import AgentHistory.Types
import AppCmd
import Array exposing (Array)
import BigInt exposing (BigInt)
import Browser
import Browser.Navigation
import CommonTypes exposing (..)
import Create.Types
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import Helpers.Eth as EthHelpers exposing (Web3Context)
import Json.Decode
import Marketplace.Types
import QuickCreate.Types
import Routing
import Time
import Trade.Types
import TradeCache.Types as TradeCache exposing (TradeCache)
import Url exposing (Url)
import UserNotice as UN exposing (UserNotice)


type alias Flags =
    { networkId : Int
    , width : Int
    , height : Int
    }


type alias Model =
    { key : Browser.Navigation.Key
    , initialWeb3State : InitialWeb3State
    , time : Time.Posix
    , web3Context : Web3Context
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , userInfo : Maybe UserInfo
    , tradeCache : TradeCache
    , submodel : Submodel
    , userNotices : List (UserNotice Msg)
    , screenWidth : Int
    }


type InitialWeb3State
    = AllGood
    | WrongNetwork
    | NoWeb3


type Submodel
    = BetaLandingPage
    | CreateModel Create.Types.Model
    | QuickCreateModel QuickCreate.Types.Model
    | TradeModel Trade.Types.Model
    | MarketplaceModel Marketplace.Types.Model
    | AgentHistoryModel AgentHistory.Types.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotoRoute Routing.Route
    | Tick Time.Posix
    | AppCmd (AppCmd.AppCmd Msg)
    | ConnectToWeb3
    | WalletStatus WalletSentry
    | NetworkUpdate Json.Decode.Value
    | TxSentryMsg TxSentry.Msg
    | UserPubkeySet Json.Decode.Value
    | CreateMsg Create.Types.Msg
    | QuickCreateMsg QuickCreate.Types.Msg
    | TradeCacheMsg TradeCache.Msg
    | TradeMsg Trade.Types.Msg
    | MarketplaceMsg Marketplace.Types.Msg
    | AgentHistoryMsg AgentHistory.Types.Msg
    | DismissNotice Int
    | NoOp
    | Test String
