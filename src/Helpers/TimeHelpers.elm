module TimeHelpers exposing (add, compare, daysStrToMaybePosix, isNegative, posixToMillisBigInt, posixToSeconds, posixToSecondsBigInt, secondsBigIntToMaybePosix, sub, toString)

import BigInt exposing (BigInt)
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
