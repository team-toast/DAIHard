module Search.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import CommonTypes exposing (UserInfo)
import Contracts.Types
import Contracts.Wrappers
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import PaymentMethods exposing (PaymentMethod)
import Routing
import Search.Types exposing (..)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


init : EthHelpers.EthNode -> Address -> Int -> Maybe Contracts.Types.OpenMode -> Maybe UserInfo -> ( Model, Cmd Msg )
init ethNode factoryAddress tokenDecimals maybeOpenMode userInfo =
    let
        openMode =
            maybeOpenMode |> Maybe.withDefault Contracts.Types.SellerOpened
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , factoryAddress = factoryAddress
      , tokenDecimals = tokenDecimals
      , numTrades = Nothing
      , openMode = openMode
      , trades = Array.empty
      , inputs = initialInputs
      , searchTerms = []
      , filterFunc = initialFilterFunc openMode
      , sortFunc = initialSortFunc
      }
    , Contracts.Wrappers.getNumTradesCmd ethNode factoryAddress NumTradesFetched
    )


initialFilterFunc : Contracts.Types.OpenMode -> (Time.Posix -> Contracts.Types.FullTradeInfo -> Bool)
initialFilterFunc openMode =
    \time trade ->
        (trade.state.phase == Contracts.Types.Open)
            && (trade.parameters.openMode == openMode)
            && (TimeHelpers.compare trade.derived.phaseEndTime time == GT)


initialSortFunc : Contracts.Types.FullTradeInfo -> Contracts.Types.FullTradeInfo -> Order
initialSortFunc a b =
    compare a.creationInfo.blocknum b.creationInfo.blocknum


initialInputs : SearchInputs
initialInputs =
    { paymentMethod = ""
    , currencyType = ""
    , showCurrencyDropdown = False
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

        PaymentMethodInputChanged input ->
            ( { model | inputs = model.inputs |> updatePaymentMethodInput input }
            , Cmd.none
            , Nothing
            )

        CurrencyInputChanged input ->
            ( { model | inputs = model.inputs |> updateCurrencyTypeInput input }
            , Cmd.none
            , Nothing
            )

        ShowCurrencyDropdown flag ->
            ( { model | inputs = model.inputs |> updateShowCurrencyDropdown flag }
            , Cmd.none
            , Nothing
            )

        AddSearchTerm ->
            if model.inputs.paymentMethod == "" then
                noUpdate model

            else
                let
                    searchTerm =
                        model.inputs.paymentMethod

                    newSearchTerms =
                        List.append
                            model.searchTerms
                            [ searchTerm ]

                    newModel =
                        { model
                            | inputs = model.inputs |> updatePaymentMethodInput ""
                            , searchTerms = newSearchTerms
                        }
                in
                ( newModel |> updateFilterFunc
                , Cmd.none
                , Nothing
                )

        ResetSearch ->
            let
                newModel =
                    { model
                        | searchTerms = []
                        , sortFunc = initialSortFunc
                        , filterFunc = initialFilterFunc model.openMode
                        , inputs = initialInputs
                    }
            in
            ( newModel, Cmd.none, Nothing )

        TradeClicked id ->
            ( model, Cmd.none, Just (Routing.Interact (Just id)) )

        SortBy colType ascending ->
            let
                newSortFunc =
                    (\a b ->
                        case colType of
                            Expiring ->
                                TimeHelpers.compare a.derived.phaseEndTime b.derived.phaseEndTime

                            TradeAmount ->
                                TokenValue.compare a.parameters.tradeAmount b.parameters.tradeAmount

                            Fiat ->
                                FiatValue.compare a.parameters.fiatPrice b.parameters.fiatPrice

                            Margin ->
                                Maybe.map2
                                    (\marginA marginB -> compare marginA marginB)
                                    a.derived.margin
                                    b.derived.margin
                                    |> Maybe.withDefault EQ

                            PaymentMethods ->
                                let
                                    _ =
                                        Debug.log "Can't sort by payment methods. What does that even mean??" ""
                                in
                                initialSortFunc a b

                            AutoabortWindow ->
                                TimeHelpers.compare a.parameters.autoabortInterval b.parameters.autoabortInterval

                            AutoreleaseWindow ->
                                TimeHelpers.compare a.parameters.autoreleaseInterval b.parameters.autoreleaseInterval
                    )
                        |> (if ascending then
                                flip

                            else
                                identity
                           )
            in
            ( { model | sortFunc = newSortFunc }
            , Cmd.none
            , Nothing
            )

        ResolveDropdowns ->
            ( { model | inputs = model.inputs |> updateShowCurrencyDropdown False }
            , Cmd.none
            , Nothing
            )

        NoOp ->
            noUpdate model


updateFilterFunc : Model -> Model
updateFilterFunc model =
    let
        newFilterFunc =
            case model.searchTerms of
                [] ->
                    initialFilterFunc model.openMode

                terms ->
                    \time trade ->
                        initialFilterFunc model.openMode
                            time
                            trade
                            && testTextMatch terms trade.parameters.paymentMethods
    in
    { model | filterFunc = newFilterFunc }


testTextMatch : List String -> List PaymentMethod -> Bool
testTextMatch terms paymentMethods =
    paymentMethods
        |> List.any
            (\pm ->
                let
                    searchable =
                        case pm of
                            PaymentMethods.CashDrop s ->
                                s

                            PaymentMethods.CashHandoff s ->
                                s

                            PaymentMethods.BankTransfer identifier ->
                                identifier.info

                            PaymentMethods.Custom s ->
                                s
                in
                terms
                    |> List.all
                        (\term ->
                            String.contains term searchable
                        )
            )


noUpdate : Model -> ( Model, Cmd Msg, Maybe Routing.Route )
noUpdate model =
    ( model, Cmd.none, Nothing )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
