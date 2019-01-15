module Interact exposing (Model, Msg(..), ToastytradeSellModel, countdownView, init, toastytradeModelFromGetFullState, update, view)

-- Library
-- Internal

import BigInt exposing (BigInt)
import Components exposing (..)
import Contracts.ToastytradeSell as ToastytradeSell exposing (..)
import Contracts.ToastytradeSellExtras exposing (..)
import Eth
import Eth.Types exposing (..)
import Eth.Utils as EthUtils
import Html exposing (..)
import Http
import Task
import Time exposing (Time)


type alias Model =
    { ttAddress : Address
    , ttModel : Maybe ToastytradeSellModel
    }


type alias ToastytradeSellModel =
    { balance : BigInt
    , phase : Phase
    , phaseStartTime : Time
    , seller : Address
    , buyer : Maybe Address
    , buyerDeposit : BigInt
    , autorecallInterval : Time
    , depositDeadlineInterval : Time
    , autoreleaseInterval : Time
    }


init : ( EthNode, Address ) -> ( Model, Cmd Msg )
init ( node, ttAddress ) =
    ( { ttAddress = ttAddress
      , ttModel = Nothing
      }
    , Eth.call node.http (ToastytradeSell.getFullState ttAddress)
        |> Task.attempt FullStateFetched
    )


type Msg
    = FullStateFetched (Result Http.Error GetFullState)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        FullStateFetched (Err e) ->
            let
                _ =
                    Debug.log "can't fetch full state: " e
            in
            ( model, Cmd.none )

        FullStateFetched (Ok getFullState) ->
            let
                ttModelResult =
                    toastytradeModelFromGetFullState getFullState
            in
            case ttModelResult of
                Err e ->
                    let
                        _ =
                            Debug.log e
                    in
                    ( model, Cmd.none )

                Ok ttModel ->
                    ( { model
                        | ttModel = Just ttModel
                      }
                    , Cmd.none
                    )


view : Model -> Time -> Html Msg
view model currentTime =
    case model.ttModel of
        Nothing ->
            div [] [ text "Model loading... " ]

        Just ttModel ->
            div []
                [ div []
                    [ text "Toastytrade Sell at address "
                    , text (EthUtils.addressToString model.ttAddress)
                    ]
                , text (toString ttModel.phase)
                , case ttModel.phase of
                    Open ->
                        div []
                            [ text (EthUtils.addressToString ttModel.seller)
                            , text " selling."
                            ]

                    _ ->
                        div []
                            [ text (EthUtils.addressToString ttModel.seller)
                            , text " selling to "
                            , text (Maybe.withDefault "???" (Maybe.map EthUtils.addressToString ttModel.buyer))
                            ]
                , countdownView ttModel currentTime
                ]


countdownView : ToastytradeSellModel -> Time.Time -> Html Msg
countdownView ttModel currentTime =
    let
        phaseInterval =
            case ttModel.phase of
                Open ->
                    ttModel.autorecallInterval

                Committed ->
                    ttModel.depositDeadlineInterval

                Claimed ->
                    ttModel.autoreleaseInterval

                Closed ->
                    0

        timeLeft =
            (ttModel.phaseStartTime + phaseInterval) - currentTime

        countdownHasPassed =
            timeLeft < 0

        textElement =
            text
                (case ( ttModel.phase, countdownHasPassed ) of
                    ( Open, False ) ->
                        "This offer autorecalls in "

                    ( Open, True ) ->
                        "Autorecall available. Poke to autorecall."

                    ( Committed, False ) ->
                        "Time left until deposit deadline: "

                    ( Committed, True ) ->
                        "Deposit deadline passed. Poke to return Seller's ether and burn Buyer's deposit."

                    ( Claimed, False ) ->
                        "Autorelease to Buyer in "

                    ( Claimed, True ) ->
                        "Autorelease available. Poke to autorelease."

                    ( Closed, _ ) ->
                        "This offer is now closed."
                )

        dynamicElement =
            if ttModel.phase == Closed then
                div [] []

            else if countdownHasPassed then
                div [] [ text "POKEBUTTON" ]

            else
                div [] [ text (toString timeLeft) ]
    in
    div [] [ textElement, dynamicElement ]


toastytradeModelFromGetFullState : ToastytradeSell.GetFullState -> Result String ToastytradeSellModel
toastytradeModelFromGetFullState fullState =
    let
        phaseResult =
            bigIntToPhase fullState.phase

        phaseStartTimeResult =
            bigIntToTime fullState.phaseStartTimestamp

        autorecallIntervalResult =
            bigIntToTime fullState.autorecallInterval

        depositDeadlineIntervalResult =
            bigIntToTime fullState.depositDeadlineInterval

        autoreleaseIntervalResult =
            bigIntToTime fullState.autoreleaseInterval
    in
    case ( phaseResult, phaseStartTimeResult, autorecallIntervalResult, depositDeadlineIntervalResult, autoreleaseIntervalResult ) of
        ( Err e, _, _, _, _ ) ->
            Err ("Error interpreting phase: " ++ e)

        ( _, Err e, _, _, _ ) ->
            Err ("Error interpreting phaseStartTimestamp: " ++ e)

        ( _, _, Err e, _, _ ) ->
            Err ("Error interpreting autorecallInterval: " ++ e)

        ( _, _, _, Err e, _ ) ->
            Err ("Error interpreting depositDeadlineInterval: " ++ e)

        ( _, _, _, _, Err e ) ->
            Err ("Error interpreting autoreleaseInterval: " ++ e)

        ( Ok phase, Ok phaseStartTime, Ok autorecallInterval, Ok depositDeadlineInterval, Ok autoreleaseInterval ) ->
            Ok
                { balance = fullState.balance
                , phase = phase
                , phaseStartTime = phaseStartTime
                , seller = fullState.seller
                , buyer = addressIfNot0x0 fullState.buyer
                , buyerDeposit = fullState.buyerDeposit
                , autorecallInterval = autorecallInterval
                , depositDeadlineInterval = depositDeadlineInterval
                , autoreleaseInterval = autoreleaseInterval
                }
