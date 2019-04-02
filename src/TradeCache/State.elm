module TradeCache.State exposing (init, initAndStartCaching, loadedTrades, startCaching, update)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import Contracts.Types as CTypes
import Contracts.Wrappers
import Dict exposing (Dict)
import Eth.Sentry.Event as EventSentry
import EthHelpers
import PaymentMethods exposing (PaymentMethod)
import Time
import TradeCache.Types exposing (..)


init : EthHelpers.EthNode -> ( TradeCache, Cmd Msg )
init ethNode =
    let
        ( sentry, sentryCmd ) =
            EventSentry.init
                EventSentryMsg
                ethNode.http
    in
    ( { ethNode = ethNode
      , eventSentry = sentry
      , numTrades = Nothing
      , trades = Array.empty
      }
    , sentryCmd
    )


startCaching : TradeCache -> Cmd Msg
startCaching tradeCache =
    Contracts.Wrappers.getNumTradesCmd tradeCache.ethNode NumTradesFetched


initAndStartCaching : EthHelpers.EthNode -> ( TradeCache, Cmd Msg )
initAndStartCaching ethNode =
    let
        ( tc, cmd1 ) =
            init ethNode
    in
    ( tc
    , Cmd.batch
        [ cmd1
        , startCaching tc
        ]
    )


update : Msg -> TradeCache -> ( TradeCache, Cmd Msg )
update msg prevModel =
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
                                                    Contracts.Wrappers.getCreationInfoFromIdCmd prevModel.ethNode (BigInt.fromInt id) (CreationInfoFetched id)
                                                )
                                        )

                                trades =
                                    List.range 0 (numTrades - 1)
                                        |> List.map CTypes.partialTradeInfo
                                        |> Array.fromList
                            in
                            ( { prevModel
                                | numTrades = Just numTrades
                                , trades = trades
                              }
                            , fetchCreationInfoCmd
                            )

                        Nothing ->
                            let
                                _ =
                                    Debug.log "can't convert the numTrades bigInt value to an int"
                            in
                            ( prevModel, Cmd.none )

                Err errstr ->
                    let
                        _ =
                            Debug.log "can't fetch numTrades:" errstr
                    in
                    ( prevModel, Cmd.none )

        CreationInfoFetched id fetchResult ->
            case fetchResult of
                Ok encodedCreationInfo ->
                    let
                        creationInfo =
                            CTypes.TradeCreationInfo
                                encodedCreationInfo.address_
                                (BigIntHelpers.toIntWithWarning encodedCreationInfo.blocknum)

                        newModel =
                            prevModel
                                |> updateTradeCreationInfo id creationInfo

                        ( newSentry, sentryCmd ) =
                            Contracts.Wrappers.getOpenedEventDataSentryCmd prevModel.eventSentry creationInfo (OpenedEventDataFetched id)

                        cmd =
                            Cmd.batch
                                [ Contracts.Wrappers.getParametersAndStateCmd prevModel.ethNode creationInfo.address (ParametersFetched id) (StateFetched id)
                                , sentryCmd
                                ]
                    in
                    ( { newModel | eventSentry = newSentry }
                    , cmd
                    )

                Err errstr ->
                    let
                        _ =
                            Debug.log ("Error fetching address on id" ++ String.fromInt id) errstr
                    in
                    ( prevModel, Cmd.none )

        ParametersFetched id fetchResult ->
            case fetchResult of
                Ok (Ok parameters) ->
                    ( prevModel |> updateTradeParameters id parameters
                    , Cmd.none
                    )

                badResult ->
                    let
                        _ =
                            Debug.log "bad parameters Fetched result" badResult
                    in
                    ( prevModel, Cmd.none )

        StateFetched id fetchResult ->
            case fetchResult of
                Ok (Just state) ->
                    ( prevModel |> updateTradeState id state
                    , Cmd.none
                    )

                _ ->
                    let
                        _ =
                            EthHelpers.logBadFetchResultMaybe fetchResult
                    in
                    ( prevModel, Cmd.none )

        OpenedEventDataFetched id fetchResult ->
            case fetchResult of
                Ok openedEventData ->
                    let
                        newModel =
                            case PaymentMethods.decodePaymentMethodList openedEventData.fiatTransferMethods of
                                Ok paymentMethods ->
                                    prevModel |> updateTradePaymentMethods id paymentMethods

                                Err e ->
                                    let
                                        _ =
                                            Debug.log "Error decoding payment methods" e
                                    in
                                    prevModel
                    in
                    ( newModel
                    , Cmd.none
                    )

                Err decodeError ->
                    let
                        _ =
                            Debug.log "Error decoding the fetched result of OpenEvent data" decodeError
                    in
                    ( prevModel
                    , Cmd.none
                    )

        EventSentryMsg eventMsg ->
            let
                ( newEventSentry, cmd ) =
                    EventSentry.update
                        eventMsg
                        prevModel.eventSentry
            in
            ( { prevModel
                | eventSentry =
                    newEventSentry
              }
            , cmd
            )


loadedTrades : TradeCache -> List CTypes.FullTradeInfo
loadedTrades tradeCache =
    tradeCache.trades
        |> Array.toList
        |> List.filterMap
            (\trade ->
                case trade of
                    CTypes.LoadedTrade fullTrade ->
                        Just fullTrade

                    _ ->
                        Nothing
            )



--currently not exposed/used


loadedTradesDict : TradeCache -> Dict Int CTypes.FullTradeInfo
loadedTradesDict tradeCache =
    tradeCache.trades
        |> Array.toList
        |> List.indexedMap Tuple.pair
        |> List.filterMap
            (\( id, trade ) ->
                case trade of
                    CTypes.LoadedTrade fullTrade ->
                        Just ( id, fullTrade )

                    _ ->
                        Nothing
            )
        |> Dict.fromList


updateTradeCreationInfo : Int -> CTypes.TradeCreationInfo -> TradeCache -> TradeCache
updateTradeCreationInfo id creationInfo tradeCache =
    case Array.get id tradeCache.trades of
        Just trade ->
            let
                newTrade =
                    CTypes.updateCreationInfo creationInfo trade

                newTradeArray =
                    Array.set id
                        newTrade
                        tradeCache.trades
            in
            { tradeCache | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTradeAddress ran into an out-of-range error" ""
            in
            tradeCache


updateTradeParameters : Int -> CTypes.TradeParameters -> TradeCache -> TradeCache
updateTradeParameters id parameters tradeCache =
    case Array.get id tradeCache.trades of
        Just trade ->
            let
                newTrade =
                    CTypes.updateParameters parameters trade

                newTradeArray =
                    Array.set id
                        newTrade
                        tradeCache.trades
            in
            { tradeCache | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTParameters ran into an out-of-range error" ""
            in
            tradeCache


updateTradeState : Int -> CTypes.State -> TradeCache -> TradeCache
updateTradeState id state tradeCache =
    case Array.get id tradeCache.trades of
        Just trade ->
            let
                newTrade =
                    CTypes.updateState state trade

                newTradeArray =
                    Array.set id
                        newTrade
                        tradeCache.trades
            in
            { tradeCache | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTState ran into an out-of-range error" ""
            in
            tradeCache


updateTradePaymentMethods : Int -> List PaymentMethod -> TradeCache -> TradeCache
updateTradePaymentMethods id methods tradeCache =
    case Array.get id tradeCache.trades of
        Just trade ->
            let
                newTrade =
                    CTypes.updatePaymentMethods methods trade

                newTradeArray =
                    Array.set id
                        newTrade
                        tradeCache.trades
            in
            { tradeCache | trades = newTradeArray }

        Nothing ->
            let
                _ =
                    Debug.log "updateTTPaymentMethods ran into an out-of-range error" ""
            in
            tradeCache
