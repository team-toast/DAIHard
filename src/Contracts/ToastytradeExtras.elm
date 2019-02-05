module Contracts.ToastytradeExtras exposing (FullParameters, Phase(..), UserParameters, bigIntToPhase, buildFullParameters, createSell, phaseToString, txReceiptToCreatedToastytradeSellAddress)

import Abi.Decode
import BigInt exposing (BigInt)
import Contracts.ToastytradeFactory
import Eth.Types exposing (Address, Call)
import EthHelpers
import Flip exposing (flip)
import Json.Decode
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)


type Phase
    = Open
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
            Just Open

        1 ->
            Just Committed

        2 ->
            Just Claimed

        3 ->
            Just Closed

        _ ->
            Nothing


phaseToString : Phase -> String
phaseToString phase =
    case phase of
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
    , summonFee : TokenValue
    , transferMethods : String
    , autorecallInterval : Time.Posix
    , depositDeadlineInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


type alias FullParameters =
    { initiatorAddress : Address
    , uncoiningAmount : TokenValue
    , summonFee : TokenValue
    , responderDeposit : TokenValue
    , transferMethods : String
    , autorecallInterval : Time.Posix
    , depositDeadlineInterval : Time.Posix
    , autoreleaseInterval : Time.Posix
    }


buildFullParameters : Address -> UserParameters -> FullParameters
buildFullParameters initiatorAddress userParameters =
    let
        responderDeposit =
            TokenValue.divByInt userParameters.uncoiningAmount 3
    in
    { uncoiningAmount = userParameters.uncoiningAmount
    , summonFee = userParameters.summonFee
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
        (TokenValue.getBigInt parameters.responderDeposit)
        (TimeHelpers.posixToSecondsBigInt parameters.autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.depositDeadlineInterval)
        (TimeHelpers.posixToSecondsBigInt parameters.autoreleaseInterval)
        parameters.transferMethods
