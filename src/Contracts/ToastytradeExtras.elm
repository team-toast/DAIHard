module Contracts.ToastytradeExtras exposing (Phase(..), bigIntToPhase, createSell, phaseToString)

import BigInt exposing (BigInt)
import Contracts.ToastytradeFactory
import Eth.Types exposing (Address, Call)
import EthHelpers
import Time
import TokenValue exposing (TokenValue)


type Phase
    = Open
    | Committed
    | Claimed
    | Closed


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
    let
        autorecallIntervalBigInt =
            EthHelpers.timeToBigInt autorecallInterval

        depositDeadlineIntervalBigInt =
            EthHelpers.timeToBigInt depositDeadlineInterval

        autoreleaseIntervalBigInt =
            EthHelpers.timeToBigInt autoreleaseInterval
    in
    Contracts.ToastytradeFactory.createToastytradeSell contractAddress seller valueBigInt buyerDepositBigInt autorecallIntervalBigInt depositDeadlineIntervalBigInt autoreleaseIntervalBigInt logisticsString
