module Marketplace.State exposing (init, subscriptions, update, updateUserInfo)

import Array exposing (Array)
import BigInt exposing (BigInt)
import BigIntHelpers
import CommonTypes exposing (UserInfo)
import Constants exposing (..)
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import EthHelpers
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Marketplace.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache


init : EthHelpers.EthNode -> Maybe UserInfo -> CTypes.OpenMode -> ( Model, Cmd Msg )
init ethNode userInfo openMode =
    let
        ( tradeCache, tcCmd ) =
            TradeCache.initAndStartCaching ethNode
    in
    ( { ethNode = ethNode
      , userInfo = userInfo
      , inputs = initialInputs openMode
      , showCurrencyDropdown = False
      , filterFunc = baseFilterFunc
      , sortFunc = initialSortFunc
      , tradeCache = tradeCache
      }
    , tcCmd |> Cmd.map TradeCacheMsg
    )


initialInputs : CTypes.OpenMode -> SearchInputs
initialInputs openMode =
    { minDai = ""
    , maxDai = ""
    , fiatType = ""
    , minFiat = ""
    , maxFiat = ""
    , paymentMethod = ""
    , paymentMethodTerms = []
    , openMode = openMode
    }


update : Msg -> Model -> ( Model, Cmd Msg, Maybe Routing.Route )
update msg model =
    case msg of
        -- Refresh time ->
        --     let
        --         cmd =
        --             model.trades
        --                 |> Array.toList
        --                 |> List.indexedMap
        --                     (\id trade ->
        --                         case trade of
        --                             CTypes.PartiallyLoadedTrade _ ->
        --                                 Cmd.none
        --                             CTypes.LoadedTrade info ->
        --                                 let
        --                                     address =
        --                                         info.creationInfo.address
        --                                 in
        --                                 Contracts.Wrappers.getStateCmd model.ethNode address (StateFetched id)
        --                     )
        --                 |> Cmd.batch
        --     in
        --     ( { model | time = time }
        --     , cmd
        --     , Nothing
        --     )
        ChangeOfferType newOpenMode ->
            ( { model | inputs = model.inputs |> updateOpenMode newOpenMode }
            , Cmd.none
            , Nothing
            )

        MinDaiChanged input ->
            ( { model | inputs = model.inputs |> updateMinDaiInput input }
            , Cmd.none
            , Nothing
            )

        MaxDaiChanged input ->
            ( { model | inputs = model.inputs |> updateMaxDaiInput input }
            , Cmd.none
            , Nothing
            )

        MinFiatChanged input ->
            ( { model | inputs = model.inputs |> updateMinFiatInput input }
            , Cmd.none
            , Nothing
            )

        MaxFiatChanged input ->
            ( { model | inputs = model.inputs |> updateMaxFiatInput input }
            , Cmd.none
            , Nothing
            )

        FiatTypeInputChanged input ->
            ( { model | inputs = model.inputs |> updateFiatTypeInput input }
            , Cmd.none
            , Nothing
            )

        ShowCurrencyDropdown flag ->
            let
                oldInputs =
                    model.inputs
            in
            ( { model
                | showCurrencyDropdown = flag
                , inputs =
                    model.inputs
                        |> (if flag then
                                updateFiatTypeInput ""

                            else
                                identity
                           )
              }
            , Cmd.none
            , Nothing
            )

        FiatTypeLostFocus ->
            ( { model | showCurrencyDropdown = False }
            , Cmd.none
            , Nothing
            )

        PaymentMethodInputChanged input ->
            ( { model | inputs = model.inputs |> updatePaymentMethodInput input }
            , Cmd.none
            , Nothing
            )

        AddSearchTerm ->
            ( model |> addPaymentInputTerm
            , Cmd.none
            , Nothing
            )

        RemoveTerm term ->
            ( model |> removePaymentInputTerm term
            , Cmd.none
            , Nothing
            )

        ApplyInputs ->
            ( model |> applyInputs
            , Cmd.none
            , Nothing
            )

        ResetSearch ->
            ( model |> resetSearch
            , Cmd.none
            , Nothing
            )

        TradeClicked id ->
            ( model, Cmd.none, Just (Routing.Trade id) )

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

        TradeCacheMsg tradeCacheMsg ->
            let
                ( newTradeCache, tcCmd ) =
                    TradeCache.update
                        tradeCacheMsg
                        model.tradeCache
            in
            ( { model | tradeCache = newTradeCache }
            , tcCmd |> Cmd.map TradeCacheMsg
            , Nothing
            )

        NoOp ->
            noUpdate model


addPaymentInputTerm : Model -> Model
addPaymentInputTerm model =
    if model.inputs.paymentMethod == "" then
        model

    else
        let
            searchTerm =
                model.inputs.paymentMethod

            newSearchTerms =
                List.append
                    model.inputs.paymentMethodTerms
                    [ searchTerm ]
        in
        { model
            | inputs =
                model.inputs
                    |> updatePaymentMethodInput ""
                    |> updatePaymentMethodTerms newSearchTerms
        }


removePaymentInputTerm : String -> Model -> Model
removePaymentInputTerm term model =
    let
        newTermList =
            model.inputs.paymentMethodTerms
                |> List.filter ((/=) term)
    in
    { model | inputs = model.inputs |> updatePaymentMethodTerms newTermList }


applyInputs : Model -> Model
applyInputs prevModel =
    let
        model =
            prevModel |> addPaymentInputTerm
    in
    case inputsToQuery model.inputs of
        Err errstr ->
            let
                _ =
                    Debug.log "Error applying inputs" errstr
            in
            prevModel

        Ok query ->
            let
                searchTest time trade =
                    case query.paymentMethodTerms of
                        [] ->
                            True

                        terms ->
                            testTextMatch terms trade.paymentMethods

                daiTest trade =
                    (case query.dai.min of
                        Nothing ->
                            True

                        Just min ->
                            TokenValue.compare trade.parameters.tradeAmount min /= LT
                    )
                        && (case query.dai.max of
                                Nothing ->
                                    True

                                Just max ->
                                    TokenValue.compare trade.parameters.tradeAmount max /= GT
                           )

                fiatTest trade =
                    case query.fiat of
                        Nothing ->
                            True

                        Just fiatQuery ->
                            (trade.parameters.fiatPrice.fiatType == fiatQuery.type_)
                                && (case fiatQuery.min of
                                        Nothing ->
                                            True

                                        Just min ->
                                            BigInt.compare trade.parameters.fiatPrice.amount min /= LT
                                   )
                                && (case fiatQuery.max of
                                        Nothing ->
                                            True

                                        Just max ->
                                            BigInt.compare trade.parameters.fiatPrice.amount max /= GT
                                   )

                newFilterFunc now trade =
                    baseFilterFunc now trade
                        && (trade.parameters.openMode == query.openMode)
                        && searchTest now trade
                        && daiTest trade
                        && fiatTest trade
            in
            { model
                | filterFunc = newFilterFunc
            }


inputsToQuery : SearchInputs -> Result String Query
inputsToQuery inputs =
    Result.map4
        (\minDai maxDai fiatMin fiatMax ->
            { dai =
                { min = minDai
                , max = maxDai
                }
            , fiat =
                Maybe.map
                    (\typeString ->
                        { type_ = typeString
                        , min = fiatMin
                        , max = fiatMax
                        }
                    )
                    (String.Extra.nonEmpty inputs.fiatType)
            , paymentMethodTerms =
                inputs.paymentMethodTerms
            , openMode = inputs.openMode
            }
        )
        (convertIfNonEmpty
            (TokenValue.fromString 18 >> Result.fromMaybe "")
            inputs.minDai
        )
        (convertIfNonEmpty
            (TokenValue.fromString 18 >> Result.fromMaybe "")
            inputs.maxDai
        )
        (convertIfNonEmpty
            (BigInt.fromString >> Result.fromMaybe "")
            inputs.minFiat
        )
        (convertIfNonEmpty
            (BigInt.fromString >> Result.fromMaybe "")
            inputs.maxFiat
        )


convertIfNonEmpty : (String -> Result String converted) -> String -> Result String (Maybe converted)
convertIfNonEmpty converter s =
    if s == "" then
        Ok Nothing

    else
        converter s
            |> Result.map Just


resetSearch : Model -> Model
resetSearch model =
    { model
        | sortFunc = initialSortFunc
        , filterFunc = baseFilterFunc
        , inputs = initialInputs model.inputs.openMode
    }


initialSortFunc : CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order
initialSortFunc a b =
    compare a.creationInfo.blocknum b.creationInfo.blocknum


baseFilterFunc : Time.Posix -> CTypes.FullTradeInfo -> Bool
baseFilterFunc now trade =
    (trade.state.phase == CTypes.Open)
        && (TimeHelpers.compare trade.derived.phaseEndTime now == GT)


testTextMatch : List String -> List PaymentMethod -> Bool
testTextMatch terms paymentMethods =
    let
        searchForAllTerms searchable =
            terms
                |> List.all
                    (\term ->
                        String.contains term searchable
                    )
    in
    paymentMethods
        |> List.any
            (\method ->
                searchForAllTerms method.info
            )


noUpdate : Model -> ( Model, Cmd Msg, Maybe Routing.Route )
noUpdate model =
    ( model, Cmd.none, Nothing )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
