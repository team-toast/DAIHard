module Interact exposing (Model, Msg(..), ToastytradeSellModel, countdownView, init, toastytradeModelFromGetFullState, update, updateWithUserAddress, view)

-- Library
-- Internal

import BigInt exposing (BigInt)
import Contracts.ToastytradeExtras exposing (..)
import Contracts.ToastytradeSell as ToastytradeSell exposing (..)
import ElementHelpers exposing (..)
import Eth
import Eth.Types exposing (..)
import Eth.Utils as EthUtils
import EthHelpers
import Flip exposing (flip)
import Html exposing (..)
import Http
import Maybe.Extra
import Task
import Time
import TimeHelpers


type alias Model =
    { ttAddress : Address
    , ttModel : Maybe ToastytradeSellModel
    }


type alias ToastytradeSellModel =
    { balance : BigInt
    , phase : Phase
    , phaseStartTime : Time.Posix
    , seller : Address
    , buyer : Maybe Address
    , buyerDeposit : BigInt
    , autorecallInterval : Time.Posix
    , depositDeadlineInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


init : ( EthHelpers.EthNode, Address ) -> ( Model, Cmd Msg )
init ( node, ttAddress ) =
    ( { ttAddress = ttAddress
      , ttModel = Nothing
      }
    , Eth.call node.http (ToastytradeSell.getFullState ttAddress)
        |> Task.attempt FullStateFetched
    )


type Msg
    = FullStateFetched (Result Http.Error GetFullState)


updateWithUserAddress : Model -> Maybe Address -> Model
updateWithUserAddress model userAddress =
    --{ model | userAddress = userAddress }
    model


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
            case toastytradeModelFromGetFullState getFullState of
                Nothing ->
                    let
                        _ =
                            Debug.log "Something went wrong while fetching Toastytrade state"
                    in
                    ( model, Cmd.none )

                Just ttModel ->
                    ( { model
                        | ttModel = Just ttModel
                      }
                    , Cmd.none
                    )


view : Model -> Time.Posix -> Html Msg
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
                , text (phaseToString ttModel.phase)
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


countdownView : ToastytradeSellModel -> Time.Posix -> Html Msg
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
                    Time.millisToPosix 0

        timeLeft =
            TimeHelpers.add ttModel.phaseStartTime phaseInterval
                |> flip TimeHelpers.sub currentTime

        countdownHasPassed =
            TimeHelpers.isNegative timeLeft

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
                div [] [ text (String.fromInt (Time.posixToMillis timeLeft // 1000)) ]
    in
    div [] [ textElement, dynamicElement ]


toastytradeModelFromGetFullState : ToastytradeSell.GetFullState -> Maybe ToastytradeSellModel
toastytradeModelFromGetFullState fullState =
    let
        phaseResult =
            bigIntToPhase fullState.phase

        phaseStartTimeResult =
            EthHelpers.bigIntToTime fullState.phaseStartTimestamp

        autorecallIntervalResult =
            EthHelpers.bigIntToTime fullState.autorecallInterval

        depositDeadlineIntervalResult =
            EthHelpers.bigIntToTime fullState.depositDeadlineInterval

        autoreleaseIntervalResult =
            EthHelpers.bigIntToTime fullState.autoreleaseInterval
    in
    Maybe.map5 (toastytradeModelFromGetFullStateVars fullState) phaseResult phaseStartTimeResult autorecallIntervalResult depositDeadlineIntervalResult autoreleaseIntervalResult


toastytradeModelFromGetFullStateVars : ToastytradeSell.GetFullState -> Phase -> Time.Posix -> Time.Posix -> Time.Posix -> Time.Posix -> ToastytradeSellModel
toastytradeModelFromGetFullStateVars fullState phase phaseStartTime autorecallInterval depositDeadlineInterval autoreleaseInterval =
    { balance = fullState.balance
    , phase = phase
    , phaseStartTime = phaseStartTime
    , seller = fullState.seller
    , buyer = EthHelpers.addressIfNot0x0 fullState.buyer
    , buyerDeposit = fullState.buyerDeposit
    , autorecallInterval = autorecallInterval
    , depositDeadlineInterval = depositDeadlineInterval
    , autoreleaseInterval = autoreleaseInterval
    }
