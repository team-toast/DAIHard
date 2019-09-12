module Marketplace.Types exposing (Errors, Model, Msg(..), Query, SearchInputs, TokenRange, UpdateResult, justModelUpdate, noErrors, updateFiatTypeInput, updateMaxDaiInput, updateMinDaiInput, updatePaymentMethodInput, updatePaymentMethodTerms)

import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies exposing (Price)
import Dict exposing (Dict)
import Eth.Net
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import Filters.Types as Filters
import Helpers.Eth as EthHelpers
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import PriceFetch
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import Wallet


type alias Model =
    { wallet : Wallet.State
    , inputs : SearchInputs
    , errors : Errors
    , showCurrencyDropdown : Bool
    , tradeTable : TradeTable.Model
    , filters : Filters.Model
    , filterFunc : Time.Posix -> CTypes.FullTradeInfo -> Bool
    , prices : List ( Currencies.Symbol, PriceFetch.PriceData )
    , now : Time.Posix
    }


type Msg
    = UpdateNow Time.Posix
    | Refresh
    | PricesFetched (Result Http.Error (List ( Currencies.Symbol, PriceFetch.PriceAndTimestamp )))
    | MinDaiChanged String
    | MaxDaiChanged String
    | FiatTypeInputChanged String
    | FiatTypeSelected String
    | PaymentMethodInputChanged String
    | ShowCurrencyDropdown Bool
    | FiatTypeLostFocus
    | AddSearchTerm
    | RemoveTerm String
    | ApplyInputs
    | ResetSearch
    | TradeTableMsg TradeTable.Msg
    | FiltersMsg Filters.Msg
    | CmdUp (CmdUp Msg)
    | NoOp


type alias UpdateResult =
    { model : Model
    , cmd : Cmd Msg
    , chainCmd : ChainCmd Msg
    , cmdUps : List (CmdUp Msg)
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
    , fiatType : Currencies.Symbol
    , paymentMethod : String
    , paymentMethodTerms : List String
    }


type alias Errors =
    { minDai : Maybe String
    , maxDai : Maybe String
    }


noErrors =
    Errors Nothing Nothing


type alias Query =
    { dai : TokenRange
    , fiatSymbol : Maybe Currencies.Symbol
    , paymentMethodTerms : List String
    }


type alias TokenRange =
    { min : Maybe TokenValue
    , max : Maybe TokenValue
    }


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
