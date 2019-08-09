module Marketplace.Types exposing (Errors, Model, Msg(..), ResultColumnType(..), SearchInputs, UpdateResult, justModelUpdate, noErrors, updateFiatTypeInput, updateMaxDaiInput, updateMinDaiInput, updatePaymentMethodInput, updatePaymentMethodTerms)

import AppCmd exposing (AppCmd)
import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import Wallet


type alias Model =
    { wallet : Wallet.State
    , tradeTable : TradeTable.Model
    , inputs : SearchInputs
    , errors : Errors
    , showCurrencyDropdown : Bool
    , filterFunc : Time.Posix -> CTypes.FullTradeInfo -> Bool
    }


type Msg
    = MinDaiChanged String
    | MaxDaiChanged String
    | FiatTypeInputChanged String
    | PaymentMethodInputChanged String
    | ShowCurrencyDropdown Bool
    | FiatTypeLostFocus
    | AddSearchTerm
    | RemoveTerm String
      -- | ApplyInputs
    | ResetSearch
    | TradeTableMsg TradeTable.Msg
    | AppCmd (AppCmd Msg)
    | NoOp



--| StateFetched Int (Result Http.Error (Maybe CTypes.State))
--| Refresh Time.Posix


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , appCmds : List (AppCmd Msg)
    }


justModelUpdate : Model -> UpdateResult
justModelUpdate model =
    UpdateResult
        model
        Cmd.none
        ChainCmd.none
        []


type alias SearchInputs =
    { minDai : String
    , maxDai : String
    , fiatType : String
    , paymentMethod : String
    , paymentMethodTerms : List String
    }


type alias Errors =
    { minDai : Maybe String
    , maxDai : Maybe String
    }


noErrors =
    Errors Nothing Nothing



-- type alias Query =
--     { dai : TokenRange
--     , fiat : Maybe FiatTypeAndRange
--     , paymentMethodTerms : List String
--     }
-- type alias TokenRange =
--     { min : Maybe TokenValue
--     , max : Maybe TokenValue
--     }
-- type alias FiatTypeAndRange =
--     { type_ : String
--     , min : Maybe BigInt
--     , max : Maybe BigInt
--     }


updatePaymentMethodInput : String -> SearchInputs -> SearchInputs
updatePaymentMethodInput input inputs =
    { inputs | paymentMethod = input }


updateFiatTypeInput : String -> SearchInputs -> SearchInputs
updateFiatTypeInput input inputs =
    { inputs | fiatType = input }


updateMinDaiInput : String -> SearchInputs -> SearchInputs
updateMinDaiInput input inputs =
    { inputs | minDai = input }


updateMaxDaiInput : String -> SearchInputs -> SearchInputs
updateMaxDaiInput input inputs =
    { inputs | maxDai = input }


updatePaymentMethodTerms : List String -> SearchInputs -> SearchInputs
updatePaymentMethodTerms terms inputs =
    { inputs | paymentMethodTerms = terms }


type ResultColumnType
    = Expiring
    | TradeAmount
    | Fiat
    | Margin
    | PaymentMethods
    | AutoabortWindow
    | AutoreleaseWindow
