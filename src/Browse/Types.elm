module Browse.Types exposing (Model, Msg(..), TTListItem, updateTTAddress, updateTTParameters, updateTTState)

import Array exposing (Array)
import BigInt exposing (BigInt)
import Contracts.Types
import Eth.Types exposing (Address)
import EthHelpers
import Http


type alias Model =
    { ethNode : EthHelpers.EthNode
    , userAddress : Maybe Address
    , factoryAddress : Address
    , tokenDecimals : Int
    , numTTs : Maybe Int
    , ttArray : Array TTListItem
    }


type Msg
    = NumTTsFetched (Result Http.Error BigInt)
    | AddressFetched Int (Result Http.Error Address)
    | ParametersFetched Int (Result Http.Error (Maybe Contracts.Types.FullParameters))
    | StateFetched Int (Result Http.Error (Maybe Contracts.Types.State))
    | ItemClicked Int


type alias TTListItem =
    { id : Int
    , address : Maybe Address
    , parameters : Maybe Contracts.Types.FullParameters
    , state : Maybe Contracts.Types.State
    }


updateTTAddress : Int -> Maybe Address -> Model -> Model
updateTTAddress id address model =
    case Array.get id model.ttArray of
        Just oldTTItem ->
            let
                newTTArray =
                    Array.set id
                        { oldTTItem | address = address }
                        model.ttArray
            in
            { model | ttArray = newTTArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTAddress ran into an out-of-range error" ""
            in
            model


updateTTParameters : Int -> Maybe Contracts.Types.FullParameters -> Model -> Model
updateTTParameters id parameters model =
    case Array.get id model.ttArray of
        Just oldTTItem ->
            let
                newTTArray =
                    Array.set id
                        { oldTTItem | parameters = parameters }
                        model.ttArray
            in
            { model | ttArray = newTTArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTParameters ran into an out-of-range error" ""
            in
            model


updateTTState : Int -> Maybe Contracts.Types.State -> Model -> Model
updateTTState id state model =
    case Array.get id model.ttArray of
        Just oldTTItem ->
            let
                newTTArray =
                    Array.set id
                        { oldTTItem | state = state }
                        model.ttArray
            in
            { model | ttArray = newTTArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTState ran into an out-of-range error" ""
            in
            model
