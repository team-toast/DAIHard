module Contracts.ToastytradeSellExtras exposing (Phase(..), bigIntToPhase, phaseToString)

-- Library

import BigInt exposing (BigInt)


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
