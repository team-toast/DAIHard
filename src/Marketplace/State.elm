module Marketplace.State exposing (init, subscriptions, update, updateUserInfo, updateWeb3Context)

import AppCmd
import Array exposing (Array)
import BigInt exposing (BigInt)
import CommonTypes exposing (..)
import Config
import Contracts.Types as CTypes
import Contracts.Wrappers
import Eth.Sentry.Event as EventSentry exposing (EventSentry)
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Flip exposing (flip)
import Helpers.BigInt as BigIntHelpers
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Marketplace.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Routing
import String.Extra
import Time
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)


init : EthHelpers.Web3Context -> BuyerOrSeller -> Maybe UserInfo -> ( Model, Cmd Msg )
init web3Context browsingRole maybeUserInfo =
    ( { web3Context = web3Context
      , userInfo = maybeUserInfo
      , browsingRole = browsingRole
      , inputs = initialInputs
      , errors = noErrors
      , showCurrencyDropdown = False
      , filterFunc = baseFilterFunc
      , sortFunc = initialSortFunc
      }
        |> applyInputs
    , Cmd.none
    )


initialInputs : SearchInputs
initialInputs =
    { minDai = ""
    , maxDai = ""
    , fiatType = ""
    , minFiat = ""
    , maxFiat = ""
    , paymentMethod = ""
    , paymentMethodTerms = []
    }


update : Msg -> Model -> UpdateResult
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
        --                                 Contracts.Wrappers.getStateCmd model.web3Context address (StateFetched id)
        --                     )
        --                 |> Cmd.batch
        --     in
        --     ( { model | time = time }
        --     , cmd
        --     , Nothing
        --     )
        MinDaiChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updateMinDaiInput input }
                Cmd.none
                []

        MaxDaiChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updateMaxDaiInput input }
                Cmd.none
                []

        MinFiatChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updateMinFiatInput input }
                Cmd.none
                []

        MaxFiatChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updateMaxFiatInput input }
                Cmd.none
                []

        FiatTypeInputChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updateFiatTypeInput input }
                Cmd.none
                []

        ShowCurrencyDropdown flag ->
            let
                oldInputs =
                    model.inputs
            in
            UpdateResult
                { model
                    | showCurrencyDropdown = flag
                    , inputs =
                        model.inputs
                            |> (if flag then
                                    updateFiatTypeInput ""

                                else
                                    identity
                               )
                }
                Cmd.none
                []

        FiatTypeLostFocus ->
            UpdateResult
                { model | showCurrencyDropdown = False }
                Cmd.none
                []

        PaymentMethodInputChanged input ->
            UpdateResult
                { model | inputs = model.inputs |> updatePaymentMethodInput input }
                Cmd.none
                []

        AddSearchTerm ->
            UpdateResult
                (model |> addPaymentInputTerm)
                Cmd.none
                []

        RemoveTerm term ->
            UpdateResult
                (model |> removePaymentInputTerm term)
                Cmd.none
                []

        ApplyInputs ->
            UpdateResult
                (model |> applyInputs)
                Cmd.none
                []

        ResetSearch ->
            UpdateResult
                (model |> resetSearch)
                Cmd.none
                []

        TradeClicked id ->
            UpdateResult
                model
                Cmd.none
                [ AppCmd.GotoRoute (Routing.Trade id) ]

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
                                FiatValue.compare a.terms.price b.terms.price

                            Margin ->
                                Maybe.map2
                                    (\marginA marginB -> compare marginA marginB)
                                    a.derived.margin
                                    b.derived.margin
                                    |> Maybe.withDefault EQ

                            -- The user shouldn't even be able to generate this message
                            PaymentMethods ->
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
            UpdateResult
                { model | sortFunc = newSortFunc }
                Cmd.none
                []

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
            |> applyInputs


removePaymentInputTerm : String -> Model -> Model
removePaymentInputTerm term model =
    let
        newTermList =
            model.inputs.paymentMethodTerms
                |> List.filter ((/=) term)
    in
    { model | inputs = model.inputs |> updatePaymentMethodTerms newTermList }
        |> applyInputs


applyInputs : Model -> Model
applyInputs prevModel =
    let
        model =
            prevModel |> addPaymentInputTerm
    in
    case inputsToQuery model.inputs of
        Err errors ->
            { prevModel | errors = errors }

        Ok query ->
            let
                searchTest time trade =
                    case query.paymentMethodTerms of
                        [] ->
                            True

                        terms ->
                            testTextMatch terms trade.terms.paymentMethods

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
                            (trade.terms.price.fiatType == fiatQuery.type_)
                                && (case fiatQuery.min of
                                        Nothing ->
                                            True

                                        Just min ->
                                            BigInt.compare trade.terms.price.amount min /= LT
                                   )
                                && (case fiatQuery.max of
                                        Nothing ->
                                            True

                                        Just max ->
                                            BigInt.compare trade.terms.price.amount max /= GT
                                   )

                newFilterFunc now trade =
                    baseFilterFunc now trade
                        && (trade.parameters.initiatingParty /= model.browsingRole)
                        && searchTest now trade
                        && daiTest trade
                        && fiatTest trade
            in
            { model
                | filterFunc = newFilterFunc
            }


inputsToQuery : SearchInputs -> Result Errors Query
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
            }
        )
        (interpretDaiAmount inputs.minDai
            |> Result.mapError (\e -> { noErrors | minDai = Just e })
        )
        (interpretDaiAmount inputs.maxDai
            |> Result.mapError (\e -> { noErrors | maxDai = Just e })
        )
        (interpretFiatAmount inputs.minFiat
            |> Result.mapError (\e -> { noErrors | minFiat = Just e })
        )
        (interpretFiatAmount inputs.maxFiat
            |> Result.mapError (\e -> { noErrors | maxFiat = Just e })
        )


interpretDaiAmount : String -> Result String (Maybe TokenValue)
interpretDaiAmount input =
    if input == "" then
        Ok Nothing

    else
        case TokenValue.fromString input of
            Nothing ->
                Err "I can't interpret this number"

            Just value ->
                Ok <| Just value


interpretFiatAmount : String -> Result String (Maybe BigInt)
interpretFiatAmount input =
    if input == "" then
        Ok Nothing

    else
        case BigInt.fromString input of
            Nothing ->
                Err "I don't understand this number."

            Just value ->
                Ok <| Just value


resetSearch : Model -> Model
resetSearch model =
    { model
        | sortFunc = initialSortFunc
        , filterFunc = baseFilterFunc
        , inputs = initialInputs
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
                        String.contains
                            (String.toLower term)
                            (String.toLower searchable)
                    )
    in
    paymentMethods
        |> List.any
            (\method ->
                searchForAllTerms method.info
            )


updateUserInfo : Maybe UserInfo -> Model -> Model
updateUserInfo userInfo model =
    { model | userInfo = userInfo }


updateWeb3Context : EthHelpers.Web3Context -> Model -> Model
updateWeb3Context newWeb3Context model =
    { model | web3Context = newWeb3Context }


subscriptions : Model -> Sub Msg
subscriptions model =
    -- Time.every 5000 Refresh
    Sub.none
