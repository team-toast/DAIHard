module Contracts.ToastytradeExtras exposing (FullParameters, Phase(..), State, UserParameters, bigIntToPhase, buildFullParameters, createSell, getParametersCmd, getStateCmd, phaseToString, txReceiptToCreatedToastytradeSellAddress)

import Abi.Decode
import BigInt exposing (BigInt)
import Contracts.ToastytradeFactory
import Contracts.ToastytradeSell as TTS
import Eth
import Eth.Types exposing (Address, Call)
import EthHelpers
import Flip exposing (flip)
import Http
import Json.Decode
import Task
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type Phase
    = Created
    | Open
    | Committed
    | Claimed
    | Closed


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
            Abi.Decode.fromString Abi.Decode.address (String.dropLeft 2 createEventData)

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


createSell : Address -> FullParameters -> Call Address
createSell contractAddress parameters =
    Contracts.ToastytradeFactory.createToastytradeSell
        contractAddress
        parameters.initiatorAddress
        (TokenValue.getBigInt parameters.uncoiningAmount)
        (TokenValue.getBigInt parameters.price)
        (TokenValue.getBigInt parameters.responderDeposit)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.depositDeadlineInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        parameters.transferMethods


type alias State =
    { balance : TokenValue
    , phase : Phase
    , phaseStartTime : Time.Posix
    , responder : Maybe Address
    }


getParametersCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe FullParameters) -> msg) -> Cmd msg
getParametersCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TTS.getParameters ttAddress)
        |> Task.map (decodeParameters numDecimals)
        |> Task.attempt msgConstructor


decodeParameters : Int -> TTS.GetParameters -> Maybe FullParameters
decodeParameters numDecimals encodedParameters =
    let
        maybeAutorecallInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autorecallInterval

        maybeDepositDeadlineInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.depositDeadlineInterval

        maybeAutoreleaseInterval =
            TimeHelpers.secondsBigIntToMaybePosix encodedParameters.autoreleaseInterval
    in
    Maybe.map3
        (\autorecallInterval depositDeadlineInterval autoreleaseInterval ->
            { uncoiningAmount = TokenValue.tokenValue numDecimals encodedParameters.sellAmount
            , price = TokenValue.tokenValue numDecimals encodedParameters.price
            , transferMethods = encodedParameters.logisticsString
            , autorecallInterval = autorecallInterval
            , depositDeadlineInterval = depositDeadlineInterval
            , autoreleaseInterval = autoreleaseInterval
            , initiatorAddress = encodedParameters.initiator
            , responderDeposit = TokenValue.tokenValue numDecimals encodedParameters.responderDeposit
            }
        )
        maybeAutorecallInterval
        maybeDepositDeadlineInterval
        maybeAutoreleaseInterval


getStateCmd : EthHelpers.EthNode -> Int -> Address -> (Result Http.Error (Maybe State) -> msg) -> Cmd msg
getStateCmd ethNode numDecimals ttAddress msgConstructor =
    Eth.call ethNode.http (TTS.getState ttAddress)
        |> Task.map (decodeState numDecimals)
        |> Task.attempt msgConstructor


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



-- mapTaskErrorToString : Task.Task Http.Error a -> Task.Task String a
-- mapTaskErrorToString task =
--     task
--         |> Task.mapError
--             (\httpError ->
--                 "Http error: "
--                     ++ httpErrorToString httpError
--             )
-- httpErrorToString : Http.Error -> String
-- httpErrorToString err =
--     case err of
--         Http.BadUrl s ->
--             "bad url: " ++ s
--         Http.Timeout ->
--             "timeout"
--         Http.NetworkError ->
--             "network error"
--         Http.BadStatus r ->
--             "bad status: " ++ r.status.message
--         Http.BadPayload s r ->
--             "bad payload: " ++ s
