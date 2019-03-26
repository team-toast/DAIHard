module Types exposing (Flags, Model(..), Msg(..), Submodel(..), ValidModel)

import BigInt exposing (BigInt)
import Browser
import Browser.Navigation
import CommonTypes exposing (UserInfo)
import Create.Types
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import EthHelpers exposing (EthNode)
import Json.Decode
import Routing
import Search.Types
import Time
import Url exposing (Url)


type alias Flags =
    { networkId : Int
    }


type Model
    = Running ValidModel
    | Failed String


type alias ValidModel =
    { key : Browser.Navigation.Key
    , time : Time.Posix
    , node : EthNode
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , userInfo : Maybe UserInfo
    , submodel : Submodel
    }


type Submodel
    = HomeModel
    | CreateModel Create.Types.Model
      --| InteractModel Interact.Types.Model
    | SearchModel Search.Types.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotoRoute Routing.Route
    | Tick Time.Posix
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | UserPubkeySet Json.Decode.Value
    | CreateMsg Create.Types.Msg
      --| InteractMsg Interact.Types.Msg
    | SearchMsg Search.Types.Msg
    | Fail String
    | NoOp
