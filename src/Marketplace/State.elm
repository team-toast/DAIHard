module Marketplace.State exposing (init, subscriptions, update, updateWalletState)

import AppCmd
import Array exposing (Array)
import BigInt exposing (BigInt)
import ChainCmd exposing (ChainCmd)
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
import TradeTable.Filters.Types as Filters
import TradeTable.State as TradeTable
import TradeTable.Types as TradeTable
import Wallet


init : Wallet.State -> ( Model, Cmd Msg )
init wallet =
    ( { wallet = wallet
      , tradeTable =
            TradeTable.init
                ( TradeTable.Expires, TradeTable.Ascending )
                [ Filters.offerType True True
                , Filters.phases True False False False
                ]
      , inputs = initialInputs
      , errors = noErrors
      , showCurrencyDropdown = False
      , filterFunc = baseFilterFunc
      }
      -- |> applyInputs
    , Cmd.none
    )


initialInputs : SearchInputs
initialInputs =
    { minDai = ""
    , maxDai = ""
    , fiatType = ""
    , paymentMethod = ""
    , paymentMethodTerms = []
    }


update : Msg -> Model -> UpdateResult
update msg prevModel =
    case msg of
        MinDaiChanged input ->
            justModelUpdate
                { prevModel | inputs = prevModel.inputs |> updateMinDaiInput input }

        MaxDaiChanged input ->
            justModelUpdate
                { prevModel | inputs = prevModel.inputs |> updateMaxDaiInput input }

        FiatTypeInputChanged input ->
            justModelUpdate
                { prevModel | inputs = prevModel.inputs |> updateFiatTypeInput input }

        ShowCurrencyDropdown flag ->
            let
                oldInputs =
                    prevModel.inputs
            in
            justModelUpdate
                { prevModel
                    | showCurrencyDropdown = flag
                    , inputs =
                        prevModel.inputs
                            |> (if flag then
                                    updateFiatTypeInput ""

                                else
                                    identity
                               )
                }

        FiatTypeLostFocus ->
            justModelUpdate
                { prevModel | showCurrencyDropdown = False }

        PaymentMethodInputChanged input ->
            justModelUpdate
                { prevModel | inputs = prevModel.inputs |> updatePaymentMethodInput input }

        AddSearchTerm ->
            justModelUpdate
                (prevModel |> addPaymentInputTerm)

        RemoveTerm term ->
            justModelUpdate
                (prevModel |> removePaymentInputTerm term)

        -- ApplyInputs ->
        --     UpdateResult
        --         (prevModel |> applyInputs)
        --         Cmd.none
        --         []
        ResetSearch ->
            justModelUpdate
                (prevModel |> resetSearch)

        TradeTableMsg tradeTableMsg ->
            let
                ttUpdateResult =
                    prevModel.tradeTable
                        |> TradeTable.update tradeTableMsg
            in
            UpdateResult
                { prevModel
                    | tradeTable = ttUpdateResult.model
                }
                (Cmd.map TradeTableMsg ttUpdateResult.cmd)
                (ChainCmd.map TradeTableMsg ttUpdateResult.chainCmd)
                (List.map (AppCmd.map TradeTableMsg) ttUpdateResult.appCmds)

        NoOp ->
            justModelUpdate prevModel

        AppCmd appCmd ->
            UpdateResult
                prevModel
                Cmd.none
                ChainCmd.none
                [ appCmd ]


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



-- |> applyInputs


removePaymentInputTerm : String -> Model -> Model
removePaymentInputTerm term model =
    let
        newTermList =
            model.inputs.paymentMethodTerms
                |> List.filter ((/=) term)
    in
    { model | inputs = model.inputs |> updatePaymentMethodTerms newTermList }



-- |> applyInputs
-- applyInputs : Model -> Model
-- applyInputs prevModel =
--     let
--         model =
--             prevModel |> addPaymentInputTerm
--     in
--     case inputsToQuery model.inputs of
--         Err errors ->
--             { prevModel | errors = errors }
--         Ok query ->
--             let
--                 searchTest time trade =
--                     case query.paymentMethodTerms of
--                         [] ->
--                             True
--                         terms ->
--                             testTextMatch terms trade.terms.paymentMethods
--                 daiTest trade =
--                     (case query.dai.min of
--                         Nothing ->
--                             True
--                         Just min ->
--                             TokenValue.compare trade.parameters.tradeAmount min /= LT
--                     )
--                         && (case query.dai.max of
--                                 Nothing ->
--                                     True
--                                 Just max ->
--                                     TokenValue.compare trade.parameters.tradeAmount max /= GT
--                            )
--                 fiatTest trade =
--                     case query.fiat of
--                         Nothing ->
--                             True
--                         Just fiatQuery ->
--                             (trade.terms.price.fiatType == fiatQuery.type_)
--                                 && (case fiatQuery.min of
--                                         Nothing ->
--                                             True
--                                         Just min ->
--                                             BigInt.compare trade.terms.price.amount min /= LT
--                                    )
--                                 && (case fiatQuery.max of
--                                         Nothing ->
--                                             True
--                                         Just max ->
--                                             BigInt.compare trade.terms.price.amount max /= GT
--                                    )
--                 newFilterFunc now trade =
--                     baseFilterFunc now trade
--                         && (trade.parameters.initiatorRole /= model.browsingRole)
--                         && searchTest now trade
--                         && daiTest trade
--                         && fiatTest trade
--             in
--             { model
--                 | filterFunc = newFilterFunc
--             }
-- inputsToQuery : SearchInputs -> Result Errors Query
-- inputsToQuery inputs =
--     Result.map4
--         (\minDai maxDai fiatMin fiatMax ->
--             { dai =
--                 { min = minDai
--                 , max = maxDai
--                 }
--             , fiat =
--                 Maybe.map
--                     (\typeString ->
--                         { type_ = typeString
--                         , min = fiatMin
--                         , max = fiatMax
--                         }
--                     )
--                     (String.Extra.nonEmpty inputs.fiatType)
--             , paymentMethodTerms =
--                 inputs.paymentMethodTerms
--             }
--         )
--         (interpretDaiAmount inputs.minDai
--             |> Result.mapError (\e -> { noErrors | minDai = Just e })
--         )
--         (interpretDaiAmount inputs.maxDai
--             |> Result.mapError (\e -> { noErrors | maxDai = Just e })
--         )
--         (interpretFiatAmount inputs.minFiat
--             |> Result.mapError (\e -> { noErrors | minFiat = Just e })
--         )
--         (interpretFiatAmount inputs.maxFiat
--             |> Result.mapError (\e -> { noErrors | maxFiat = Just e })
--         )


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
        | filterFunc = baseFilterFunc
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


updateWalletState : Wallet.State -> Model -> Model
updateWalletState wallet model =
    { model | wallet = wallet }


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none
