module Types exposing (Flags, Model(..), Msg(..), Submodel(..), ValidModel)

import BigInt exposing (BigInt)
import Browse.Types
import Browser
import Browser.Navigation
import CommonTypes exposing (UserInfo)
import Create.Types
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import EthHelpers
import Interact.Types
import Json.Decode
import Routing
import Time
import Url exposing (Url)


type alias Flags =
    { networkId : Int
    , tokenContractDecimals : Int
    , tokenContractAddressString : String
    , factoryAddressString : String
    }


type Model
    = Running ValidModel
    | Failed String


type alias ValidModel =
    { key : Browser.Navigation.Key
    , tokenContractAddress : Address
    , factoryAddress : Address
    , time : Time.Posix
    , node : EthHelpers.EthNode
    , txSentry : TxSentry Msg
    , userAddress : Maybe Address
    , userInfo : Maybe UserInfo
    , tokenContractDecimals : Int
    , submodel : Submodel
    }


type Submodel
    = HomeModel
    | CreateModel Create.Types.Model
    | InteractModel Interact.Types.Model
    | BrowseModel Browse.Types.Model


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotoRoute Routing.Route
    | Tick Time.Posix
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | UserPubkeySet Json.Decode.Value
    | MessagesEncrypted Json.Decode.Value
    | CreateMsg Create.Types.Msg
    | InteractMsg Interact.Types.Msg
    | BrowseMsg Browse.Types.Msg
    | Fail String
