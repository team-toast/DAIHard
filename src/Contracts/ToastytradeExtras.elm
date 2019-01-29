module Contracts.ToastytradeExtras exposing (Phase(..), bigIntToPhase, createSell, phaseToString, txReceiptToCreatedToastytradeSellAddress)

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


createSell : Address -> Address -> BigInt -> BigInt -> Time.Posix -> Time.Posix -> Time.Posix -> String -> Call Address
createSell contractAddress seller valueBigInt buyerDepositBigInt autorecallInterval depositDeadlineInterval autoreleaseInterval logisticsString =
    Contracts.ToastytradeFactory.createToastytradeSell
        contractAddress
        seller
        valueBigInt
        buyerDepositBigInt
        (TimeHelpers.posixToSecondsBigInt autorecallInterval)
        (TimeHelpers.posixToSecondsBigInt depositDeadlineInterval)
        (TimeHelpers.posixToSecondsBigInt autoreleaseInterval)
        logisticsString
