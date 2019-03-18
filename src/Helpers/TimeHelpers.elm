module TimeHelpers exposing (HumanReadableInterval, add, compare, daysStrToMaybePosix, getRatio, isNegative, posixToMillisBigInt, posixToSeconds, posixToSecondsBigInt, secondsBigIntToMaybePosix, sub, toHumanReadableInterval, toString)

import BigInt exposing (BigInt)
import BigIntHelpers
import Time


add : Time.Posix -> Time.Posix -> Time.Posix
add t1 t2 =
    t1
        |> Time.posixToMillis
        |> (+) (Time.posixToMillis t2)
        |> Time.millisToPosix


sub : Time.Posix -> Time.Posix -> Time.Posix
sub t1 t2 =
    t2
        |> Time.posixToMillis
        |> (-) (Time.posixToMillis t1)
        |> Time.millisToPosix


getRatio : Time.Posix -> Time.Posix -> Float
getRatio t1 t2 =
    toFloat (Time.posixToMillis t1) / toFloat (Time.posixToMillis t2)


isNegative : Time.Posix -> Bool
isNegative t =
    Time.posixToMillis t < 0


secondsBigIntToMaybePosix : BigInt -> Maybe Time.Posix
secondsBigIntToMaybePosix bigint =
    BigInt.toString bigint
        |> String.toInt
        |> Maybe.map (\t -> Time.millisToPosix (t * 1000))


daysStrToMaybePosix : String -> Maybe Time.Posix
daysStrToMaybePosix timeStr =
    let
        daysToMillis days =
            days * 24 * 60 * 60 * 1000
    in
    timeStr
        |> String.toFloat
        |> Maybe.map daysToMillis
        |> Maybe.map floor
        |> Maybe.map Time.millisToPosix


posixToSeconds : Time.Posix -> Int
posixToSeconds t =
    Time.posixToMillis t // 1000


posixToMillisBigInt : Time.Posix -> BigInt
posixToMillisBigInt t =
    Time.posixToMillis t
        |> BigInt.fromInt


posixToSecondsBigInt : Time.Posix -> BigInt
posixToSecondsBigInt t =
    BigInt.div (posixToMillisBigInt t) (BigInt.fromInt 1000)


toString : Time.Posix -> String
toString t =
    (posixToSeconds t
        |> String.fromInt
    )
        ++ " seconds"


compare : Time.Posix -> Time.Posix -> Order
compare t1 t2 =
    Basics.compare
        (Time.posixToMillis t1)
        (Time.posixToMillis t2)


type alias HumanReadableInterval =
    { days : Int
    , hours : Int
    , min : Int
    , sec : Int
    }



--ignores some maybes, because we never divmod by zero.


toHumanReadableInterval : Time.Posix -> Maybe HumanReadableInterval
toHumanReadableInterval t =
    let
        secsInDays =
            posixToSecondsBigInt t
    in
    BigInt.divmod secsInDays (BigInt.fromInt <| 60 * 60 * 24)
        |> Maybe.withDefault ( BigInt.fromInt 0, BigInt.fromInt 0 )
        |> (\( days, secsInHours ) ->
                BigInt.divmod secsInHours (BigInt.fromInt <| 60 * 60)
                    |> Maybe.withDefault ( BigInt.fromInt 0, BigInt.fromInt 0 )
                    |> (\( hours, secsInMin ) ->
                            BigInt.divmod secsInMin (BigInt.fromInt 60)
                                |> Maybe.withDefault ( BigInt.fromInt 0, BigInt.fromInt 0 )
                                |> (\( min, sec ) ->
                                        Maybe.map4
                                            HumanReadableInterval
                                            (BigIntHelpers.toInt days)
                                            (BigIntHelpers.toInt hours)
                                            (BigIntHelpers.toInt min)
                                            (BigIntHelpers.toInt sec)
                                   )
                       )
           )
