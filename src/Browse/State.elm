module Browse.State exposing (init, update, updateWithUserAddress)

import Array
import BigInt exposing (BigInt)
import BigIntHelpers
import Browse.Types exposing (..)
import Contracts.Wrappers
import Eth.Types exposing (Address)
import EthHelpers
import Routing
import Types as ParentTypes


init : EthHelpers.EthNode -> Address -> Int -> Maybe Address -> ( Model, Cmd Msg )
init ethNode factoryAddress tokenDecimals maybeUserAddress =
    ( { ethNode = ethNode
      , userAddress = maybeUserAddress
      , factoryAddress = factoryAddress
      , tokenDecimals = tokenDecimals
      , numTTs = Nothing
      , ttArray = Array.empty
      }
    , Contracts.Wrappers.getNumTTsCmd ethNode factoryAddress NumTTsFetched
    )


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Routing.Route )
update msg model =
    case msg of
        NumTTsFetched fetchResult ->
            case fetchResult of
                Ok bigInt ->
                    case BigIntHelpers.toInt bigInt of
                        Just numTTs ->
                            let
                                fetchAddressesCmd =
                                    Cmd.batch
                                        (List.range 0 (numTTs - 1)
                                            |> List.map
                                                (\id ->
                                                    Contracts.Wrappers.getAddressFromIdCmd model.ethNode model.factoryAddress (BigInt.fromInt id) (AddressFetched id)
                                                )
                                        )

                                ttArray =
                                    List.range 0 (numTTs - 1)
                                        |> List.map
                                            (\id ->
                                                { id = id
                                                , address = Nothing
                                                , parameters = Nothing
                                                , state = Nothing
                                                }
                                            )
                                        |> Array.fromList
                            in
                            ( { model
                                | numTTs = Just numTTs
                                , ttArray = ttArray
                              }
                            , fetchAddressesCmd
                            , Nothing
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "can't convert the numTTs bigInt value to an int"
                            in
                            ( model, Cmd.none, Nothing )

                Err errstr ->
                    let
                        _ =
                            Debug.log "can't fetch numTTs:" errstr
                    in
                    ( model, Cmd.none, Nothing )

        AddressFetched id fetchResult ->
            case fetchResult of
                Ok address ->
                    ( model |> updateTTAddress id (Just address)
                    , Contracts.Wrappers.getContractParametersAndStateCmd model.ethNode model.tokenDecimals address (ParametersFetched id) (StateFetched id)
                    , Nothing
                    )

                Err errstr ->
                    let
                        _ =
                            Debug.log "Error fetching address on id" id
                    in
                    ( model, Cmd.none, Nothing )

        ParametersFetched id fetchResult ->
            case fetchResult of
                Ok (Just parameters) ->
                    ( model |> updateTTParameters id (Just parameters)
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, Nothing )

        StateFetched id fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    ( model |> updateTTState id (Just state)
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, Nothing )

        ItemClicked id ->
            ( model, Cmd.none, Just (Routing.Interact (Just id)) )


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model address =
    { model | userAddress = address }
