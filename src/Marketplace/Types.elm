module Marketplace.Types exposing (Errors, FiatTypeAndRange, Model, Msg(..), Query, ResultColumnType(..), SearchInputs, TokenRange, noErrors, updateFiatTypeInput, updateMaxDaiInput, updateMaxFiatInput, updateMinDaiInput, updateMinFiatInput, updatePaymentMethodInput, updatePaymentMethodTerms)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Helpers.Eth as EthHelpers exposing (EthNode)
import Http
import Json.Decode
import PaymentMethods exposing (PaymentMethod)
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.Types as TradeCache exposing (TradeCache)


type alias Model =
    { ethNode : EthNode
    , userInfo : Maybe UserInfo
    , browsingRole : BuyerOrSeller
    , inputs : SearchInputs
    , errors : Errors
    , showCurrencyDropdown : Bool
    , filterFunc : Time.Posix -> CTypes.FullTradeInfo -> Bool
    , sortFunc : CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order
    }


type Msg
    = MinDaiChanged String
    | MaxDaiChanged String
    | FiatTypeInputChanged String
    | MinFiatChanged String
    | MaxFiatChanged String
    | PaymentMethodInputChanged String
    | ShowCurrencyDropdown Bool
    | FiatTypeLostFocus
    | AddSearchTerm
    | RemoveTerm String
    | ApplyInputs
    | ResetSearch
    | TradeClicked Int
    | SortBy ResultColumnType Bool
    | NoOp



--| StateFetched Int (Result Http.Error (Maybe CTypes.State))
--| Refresh Time.Posix


type alias SearchInputs =
    { minDai : String
    , maxDai : String
    , fiatType : String
    , minFiat : String
    , maxFiat : String
    , paymentMethod : String
    , paymentMethodTerms : List String
    }


type alias Errors =
    { minDai : Maybe String
    , maxDai : Maybe String
    , minFiat : Maybe String
    , maxFiat : Maybe String
    }


noErrors =
    Errors Nothing Nothing Nothing Nothing


type alias Query =
    { dai : TokenRange
    , fiat : Maybe FiatTypeAndRange
    , paymentMethodTerms : List String
    }


type alias TokenRange =
    { min : Maybe TokenValue
    , max : Maybe TokenValue
    }


type alias FiatTypeAndRange =
    { type_ : String
    , min : Maybe BigInt
    , max : Maybe BigInt
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


updateMinFiatInput : String -> SearchInputs -> SearchInputs
updateMinFiatInput input inputs =
    { inputs | minFiat = input }


updateMaxFiatInput : String -> SearchInputs -> SearchInputs
updateMaxFiatInput input inputs =
    { inputs | maxFiat = input }


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
