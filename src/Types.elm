module Types exposing (Flags, Model(..), Msg(..), Route(..), Submodel(..), ValidModel)

import Browser
import Browser.Navigation
import Create.Types
import Eth.Sentry.Tx as TxSentry exposing (TxSentry)
import Eth.Sentry.Wallet as WalletSentry exposing (WalletSentry)
import Eth.Types exposing (Address)
import EthHelpers
import Interact.Types
import Time
import Url exposing (Url)


type Route
    = Home
    | Create
    | Interact (Maybe String)
    | NotFound


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
    , tokenContractDecimals : Int
    , submodel : Submodel
    }


type Submodel
    = HomeModel
    | CreateModel Create.Types.Model
    | InteractModel Interact.Types.Model
    | None


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url
    | GotoRoute Route
    | Tick Time.Posix
    | WalletStatus WalletSentry
    | TxSentryMsg TxSentry.Msg
    | CreateMsg Create.Types.Msg
    | InteractMsg Interact.Types.Msg
    | Fail String
