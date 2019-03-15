module Search.Types exposing (AmountRange, FullTradeInfo, Model, Msg(..), PartialTradeInfo, ResultColumnType(..), SearchInputs, Trade(..), checkIfLoaded, updateTradeCreationInfo, updateTradeParameters, updateTradeState)

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
    | TradeClicked Int
    | SortBy ResultColumnType Bool
    | NoOp


type alias SearchInputs =
    { paymentMethod : String
    }


type alias AmountRange =
    { min : Maybe TokenValue
    , max : Maybe TokenValue
    }


type Trade
    = Loading PartialTradeInfo
    | Loaded FullTradeInfo


type alias PartialTradeInfo =
    { factoryID : Int
    , address : Maybe Address
    , parameters : Maybe Contracts.Types.CreateParameters
    , state : Maybe Contracts.Types.State
    }


type alias FullTradeInfo =
    { factoryID : Int
    , address : Address
    , parameters : Contracts.Types.CreateParameters
    , state : Contracts.Types.State
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


checkIfLoaded : PartialTradeInfo -> Trade
checkIfLoaded tradeInfo =
    case ( tradeInfo.address, tradeInfo.parameters, tradeInfo.state ) of
        ( Just address, Just parameters, Just state ) ->
            Loaded <| FullTradeInfo tradeInfo.factoryID address parameters state

        _ ->
            Loading tradeInfo
