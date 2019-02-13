module Contracts.Types exposing (FullParameters, Phase(..), State, UserParameters, bigIntToPhase, buildFullParameters, decodeState, phaseToString, txReceiptToCreatedToastytradeSellAddress, txReceiptToCreatedToastytradeSellId)

import Abi.Decode
import BigInt exposing (BigInt)
import Contracts.Generated.ToastytradeSell as TTS
import Eth.Types exposing (Address)
import EthHelpers
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type Phase
    = Created
    | Open
    | Committed
    | Claimed
    | Closed


type alias UserParameters =
    { uncoiningAmount : TokenValue
    , price : TokenValue
    , transferMethods : String
    , autorecallInterval : Time.Posix
    , depositDeadlineInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias FullParameters =
    { uncoiningAmount : TokenValue
    , price : TokenValue
    , transferMethods : String
    , autorecallInterval : Time.Posix
    , depositDeadlineInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    , initiatorAddress : Address
    , responderDeposit : TokenValue
    }


type alias State =
    { balance : TokenValue
    , phase : Phase
    , phaseStartTime : Time.Posix
    , responder : Maybe Address
    }


txReceiptToCreatedToastytradeSellAddress : Address -> Eth.Types.TxReceipt -> Result String Address
txReceiptToCreatedToastytradeSellAddress factoryAddress txReceipt =
    let
        maybeCreateEventData =
            txReceipt.logs
                |> List.filter (\log -> log.address == factoryAddress)
                |> List.head
                |> Maybe.map (\log -> log.data)
    in
    case maybeCreateEventData of
        Just createEventData ->
            createEventData
                |> String.dropLeft (2 + 64)
                |> String.left 64
                |> Abi.Decode.fromString Abi.Decode.address

        Nothing ->
            Err "Can't find a log generated from the given factoryAddress. Are you looking at the wrong transaction?"


txReceiptToCreatedToastytradeSellId : Address -> Eth.Types.TxReceipt -> Result String BigInt
txReceiptToCreatedToastytradeSellId factoryAddress txReceipt =
    let
        maybeCreateEventData =
            txReceipt.logs
                |> List.filter (\log -> log.address == factoryAddress)
                |> List.head
                |> Maybe.map (\log -> log.data)
    in
    case maybeCreateEventData of
        Just createEventData ->
            createEventData
                -- Remove the '0x' at the beginning
                |> String.dropLeft 2
                -- Take the first 64-char argument
                |> String.left 64
                |> Abi.Decode.fromString Abi.Decode.uint

        Nothing ->
            Err "Can't find a log generated from the given factoryAddress. Are you looking at the wrong transaction?"


bigIntToPhase : BigInt -> Maybe Phase
bigIntToPhase phase =
    let
        phaseInt =
            Maybe.withDefault 99 (BigInt.toString phase |> String.toInt)
    in
    case phaseInt of
        0 ->
            Just Created

        1 ->
            Just Open

        2 ->
            Just Committed

        3 ->
            Just Claimed

        4 ->
            Just Closed

        _ ->
            Nothing


phaseToString : Phase -> String
phaseToString phase =
    case phase of
        Created ->
            "Created"

        Open ->
            "Open"

        Committed ->
            "Committed"

        Claimed ->
            "Claimed"

        Closed ->
            "Closed"


buildFullParameters : Address -> UserParameters -> FullParameters
buildFullParameters initiatorAddress userParameters =
    let
        responderDeposit =
            TokenValue.divByInt userParameters.uncoiningAmount 3
    in
    { uncoiningAmount = userParameters.uncoiningAmount
    , price = userParameters.price
    , autorecallInterval = userParameters.autorecallInterval
    , depositDeadlineInterval = userParameters.depositDeadlineInterval
    , autoreleaseInterval = userParameters.autoreleaseInterval
    , transferMethods = userParameters.transferMethods
    , initiatorAddress = initiatorAddress
    , responderDeposit = responderDeposit
    }


decodeState : Int -> TTS.GetState -> Maybe State
decodeState numDecimals encodedState =
    let
        maybePhase =
            bigIntToPhase encodedState.phase

        maybePhaseStartTime =
            TimeHelpers.secondsBigIntToMaybePosix encodedState.phaseStartTimestamp
    in
    Maybe.map2
        (\phase phaseStartTime ->
            { balance = TokenValue.tokenValue numDecimals encodedState.balance
            , phase = phase
            , phaseStartTime = phaseStartTime
            , responder = EthHelpers.addressIfNot0x0 encodedState.responder
            }
        )
        maybePhase
        maybePhaseStartTime
