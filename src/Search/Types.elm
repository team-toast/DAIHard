module Search.Types exposing (Model, Msg(..), Trade, updateTradeAddress, updateTradeParameters, updateTradeState)

import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (UserInfo)
import Contracts.Generated.ToastytradeFactory as TTF
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userInfo : Maybe UserInfo
    , factoryAddress : Address
    , tokenDecimals : Int
    , numTrades : Maybe Int
    , trades : Array Trade
    }


type Msg
    = NumTradesFetched (Result Http.Error BigInt)
    | CreationInfoFetched Int (Result Http.Error TTF.CreatedTrade)
    | ParametersFetched Int (Result Http.Error (Result String Contracts.Types.CreateParameters))
    | StateFetched Int (Result Http.Error (Maybe Contracts.Types.State))
    | TradeClicked Int


type alias Trade =
    { id : Int
    , address : Maybe Address
    , parameters : Maybe Contracts.Types.CreateParameters
    , state : Maybe Contracts.Types.State
    }


updateTradeAddress : Int -> Address -> Model -> Model
updateTradeAddress id address model =
    case Array.get id model.trades of
        Just trade ->
            let
                newTradeArray =
                    Array.set id
                        { trade | address = Just address }
                        model.trades
            in
            { model | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTAddress ran into an out-of-range error" ""
            in
            model


updateTradeParameters : Int -> Contracts.Types.CreateParameters -> Model -> Model
updateTradeParameters id parameters model =
    case Array.get id model.trades of
        Just trade ->
            let
                newTradeArray =
                    Array.set id
                        { trade | parameters = Just parameters }
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
                newTradeArray =
                    Array.set id
                        { trade | state = Just state }
                        model.trades
            in
            { model | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTState ran into an out-of-range error" ""
            in
            model
