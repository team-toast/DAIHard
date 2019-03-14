module Search.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import CommonTypes exposing (UserInfo)
import Contracts.Types
import Contracts.Wrappers
import Eth.Types exposing (Address)
import EthHelpers
import Routing
import Search.Types exposing (..)
import Time
import TimeHelpers


init : EthHelpers.EthNode -> Address -> Int -> Maybe UserInfo -> ( Model, Cmd Msg )
init ethNode factoryAddress tokenDecimals userInfo =
    ( { ethNode = ethNode
      , userInfo = userInfo
      , factoryAddress = factoryAddress
      , tokenDecimals = tokenDecimals
      , numTrades = Nothing
      , trades = Array.empty
      , inputs = startingInputs
      , filterFunc = initialFilterFunc
      , sortFunc = initialSortFunc
      }
    , Contracts.Wrappers.getNumTradesCmd ethNode factoryAddress NumTradesFetched
    )


initialFilterFunc : Time.Posix -> Contracts.Types.FullTradeInfo -> Bool
initialFilterFunc time trade =
    (trade.state.phase == Contracts.Types.Open)
        && (TimeHelpers.compare trade.derived.phaseEndTime time == GT)


initialSortFunc : Contracts.Types.FullTradeInfo -> Contracts.Types.FullTradeInfo -> Order
initialSortFunc a b =
    compare a.creationInfo.blocknum b.creationInfo.blocknum


startingInputs : SearchInputs
startingInputs =
    { daiRange = AmountRange Nothing Nothing
    , fiatType = Nothing
    , fiatRange = AmountRange Nothing Nothing
    , paymentMethod = Nothing
    , location = Nothing
    }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Routing.Route )
update msg model =
    case msg of
        NumTradesFetched fetchResult ->
            case fetchResult of
                Ok bigInt ->
                    case BigIntHelpers.toInt bigInt of
                        Just numTrades ->
                            let
                                fetchCreationInfoCmd =
                                    Cmd.batch
                                        (List.range 0 (numTrades - 1)
                                            |> List.map
                                                (\id ->
                                                    Contracts.Wrappers.getCreationInfoFromIdCmd model.ethNode model.factoryAddress (BigInt.fromInt id) (CreationInfoFetched id)
                                                )
                                        )

                                trades =
                                    List.range 0 (numTrades - 1)
                                        |> List.map Contracts.Types.partialTradeInfo
                                        |> Array.fromList
                            in
                            ( { model
                                | numTrades = Just numTrades
                                , trades = trades
                              }
                            , fetchCreationInfoCmd
                            , Nothing
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "can't convert the numTrades bigInt value to an int"
                            in
                            ( model, Cmd.none, Nothing )

                Err errstr ->
                    let
                        _ =
                            Debug.log "can't fetch numTrades:" errstr
                    in
                    ( model, Cmd.none, Nothing )

        CreationInfoFetched id fetchResult ->
            case fetchResult of
                Ok creationInfo ->
                    ( model
                        |> updateTradeCreationInfo
                            id
                            (Contracts.Types.TradeCreationInfo
                                creationInfo.address_
                                (BigIntHelpers.toIntWithWarning creationInfo.blocknum)
                            )
                    , Contracts.Wrappers.getParametersAndStateCmd model.ethNode model.tokenDecimals creationInfo.address_ (ParametersFetched id) (StateFetched id)
                    , Nothing
                    )

                Err errstr ->
                    let
                        _ =
                            Debug.log ("Error fetching address on id" ++ String.fromInt id) errstr
                    in
                    ( model, Cmd.none, Nothing )

        ParametersFetched id fetchResult ->
            case fetchResult of
                Ok (Ok parameters) ->
                    ( model |> updateTradeParameters id parameters
                    , Cmd.none
                    , Nothing
                    )

                badResult ->
                    let
                        _ =
                            Debug.log "bad parametersFetched result" badResult
                    in
                    ( model, Cmd.none, Nothing )

        StateFetched id fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    ( model |> updateTradeState id state
                    , Cmd.none
                    , Nothing
                    )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( model, Cmd.none, Nothing )

        TradeClicked id ->
            ( model, Cmd.none, Just (Routing.Interact (Just id)) )

        SortBy colType ascending ->
            let
                _ =
                    Debug.log "order by" ( colType, ascending )
            in
            ( model, Cmd.none, Nothing )

        NoOp ->
            ( model, Cmd.none, Nothing )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
