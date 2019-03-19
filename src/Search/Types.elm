module Search.Types exposing (FiatTypeAndRange, Model, Msg(..), ResultColumnType(..), SearchInputs, TokenRange, updateTradeCreationInfo, updateTradeParameters, updateTradeState)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Http
import Time
import TokenValue exposing (TokenValue)


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , factoryAddress : Address
    , tokenDecimals : Int
    , numTrades : Maybe Int
    , openMode : Contracts.Types.OpenMode
    , inputs : SearchInputs
    , searchTerms : List String
    , trades : Array Contracts.Types.Trade
    , filterFunc : Time.Posix -> Contracts.Types.FullTradeInfo -> Bool
    , sortFunc : Contracts.Types.FullTradeInfo -> Contracts.Types.FullTradeInfo -> Order
    }


type Msg
    = NumTradesFetched (Result Http.Error BigInt)
    | CreationInfoFetched Int (Result Http.Error TTF.CreatedTrade)
    | ParametersFetched Int (Result Http.Error (Result String Contracts.Types.CreateParameters))
    | StateFetched Int (Result Http.Error (Maybe Contracts.Types.State))
    | SearchInputChanged String
    | AddSearchTerm
    | ResetSearch
    | TradeClicked Int
    | SortBy ResultColumnType Bool
    | NoOp


type alias SearchInputs =
    { paymentMethod : String
    }


type alias TokenRange =
    { min : Maybe TokenValue
    , max : Maybe TokenValue
    }


type alias FiatTypeAndRange =
    { type_ : Maybe FiatValue.FiatType
    , min : Maybe BigInt
    , max : Maybe BigInt
    }


type ResultColumnType
    = Expiring
    | TradeAmount
    | Fiat
    | Margin
    | PaymentMethods
    | AutoabortWindow
    | AutoreleaseWindow


updateTradeCreationInfo : Int -> Contracts.Types.TradeCreationInfo -> Model -> Model
updateTradeCreationInfo id creationInfo model =
    case Array.get id model.trades of
        Just trade ->
            let
                newTrade =
                    Contracts.Types.updateCreationInfo creationInfo trade

                newTradeArray =
                    Array.set id
                        newTrade
                        model.trades
            in
            { model | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTradeAddress ran into an out-of-range error" ""
            in
            model


updateTradeParameters : Int -> Contracts.Types.CreateParameters -> Model -> Model
updateTradeParameters id parameters model =
    case Array.get id model.trades of
        Just trade ->
            let
                newTrade =
                    Contracts.Types.updateParameters parameters trade

                newTradeArray =
                    Array.set id
                        newTrade
                        model.trades
            in
            { model | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTParameters ran into an out-of-range error" ""
            in
            model


updateTradeState : Int -> Contracts.Types.State -> Model -> Model
updateTradeState id state model =
    case Array.get id model.trades of
        Just trade ->
            let
                newTrade =
                    Contracts.Types.updateState state trade

                newTradeArray =
                    Array.set id
                        newTrade
                        model.trades
            in
            { model | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTState ran into an out-of-range error" ""
            in
            model
