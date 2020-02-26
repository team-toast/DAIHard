module Helpers.Time exposing (HumanReadableInterval, add, compare, daysStrToMaybePosix, getRatio, isNegative, monthToShortString, mul, negativeToZero, oneDay, oneHour, oneMinute, oneSecond, oneWeek, oneYear, posixToMillisBigInt, posixToSeconds, posixToSecondsBigInt, secondsBigIntToMaybePosix, secondsBigIntToPosixWithWarning, secondsToPosix, sub, toConciseIntervalString, toHumanReadableInterval, toString, weekdayToShortString)

import BigInt exposing (BigInt)
import Helpers.BigInt as BigIntHelpers
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


mul : Time.Posix -> Int -> Time.Posix
mul t i =
    t
        |> Time.posixToMillis
        |> (*) i
        |> Time.millisToPosix


negativeToZero : Time.Posix -> Time.Posix
negativeToZero t =
    if Time.posixToMillis t < 0 then
        Time.millisToPosix 0

    else
        t


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


secondsToPosix : Int -> Time.Posix
secondsToPosix s =
    Time.millisToPosix (s * 1000)


secondsBigIntToPosixWithWarning : BigInt -> Time.Posix
secondsBigIntToPosixWithWarning =
    BigIntHelpers.toIntWithWarning
        >> (\secs -> secs * 1000)
        >> Time.millisToPosix


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


toHumanReadableInterval : Time.Posix -> HumanReadableInterval
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
                                        HumanReadableInterval
                                            (BigIntHelpers.toIntWithWarning days)
                                            (BigIntHelpers.toIntWithWarning hours)
                                            (BigIntHelpers.toIntWithWarning min)
                                            (BigIntHelpers.toIntWithWarning sec)
                                   )
                       )
           )


toConciseIntervalString : Time.Posix -> String
toConciseIntervalString t =
    let
        hri =
            toHumanReadableInterval t
    in
    if hri.days > 0 then
        String.fromInt hri.days ++ "d " ++ String.fromInt hri.hours ++ "h"

    else if hri.hours > 0 then
        String.fromInt hri.hours ++ "h " ++ String.fromInt hri.min ++ "m"

    else if hri.min > 0 then
        String.fromInt hri.min ++ "m " ++ String.fromInt hri.sec ++ "s"

    else
        String.fromInt hri.sec ++ "s"


oneSecond : Time.Posix
oneSecond =
    secondsToPosix 1


oneMinute : Time.Posix
oneMinute =
    secondsToPosix 60


oneHour : Time.Posix
oneHour =
    secondsToPosix <| 60 * 60


oneDay : Time.Posix
oneDay =
    secondsToPosix <| 60 * 60 * 24


oneWeek : Time.Posix
oneWeek =
    secondsToPosix <| 60 * 60 * 24 * 7


oneYear : Time.Posix
oneYear =
    secondsToPosix 31557600


weekdayToShortString : Time.Weekday -> String
weekdayToShortString wd =
    case wd of
        Time.Mon ->
            "Mon"

        Time.Tue ->
            "Tue"

        Time.Wed ->
            "Wed"

        Time.Thu ->
            "Thu"

        Time.Fri ->
            "Fri"

        Time.Sat ->
            "Sat"

        Time.Sun ->
            "Sun"


monthToShortString : Time.Month -> String
monthToShortString m =
    case m of
        Time.Jan ->
            "Jan"

        Time.Feb ->
            "Feb"

        Time.Mar ->
            "Mar"

        Time.Apr ->
            "Apr"

        Time.May ->
            "May"

        Time.Jun ->
            "Jun"

        Time.Jul ->
            "Jul"

        Time.Aug ->
            "Aug"

        Time.Sep ->
            "Sep"

        Time.Oct ->
            "Oct"

        Time.Nov ->
            "Nov"

        Time.Dec ->
            "Dec"
