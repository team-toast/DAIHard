module BucketSale.View exposing (root)

import BigInt exposing (BigInt)
import BucketSale.Types exposing (..)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import FormatFloat exposing (formatFloat)
import Helpers.Element as EH
import Helpers.Eth as EthHelpers
import Helpers.Time as TimeHelpers
import Images
import List.Extra
import Maybe.Extra
import Result.Extra
import Routing
import Time
import TokenValue exposing (TokenValue)
import Wallet


root : DisplayProfile -> Model -> ( Element Msg, List (Element Msg) )
root dProfile model =
    ( Element.column
        [ Element.width Element.fill
        , Element.paddingEach
            { bottom = 40
            , top = 0
            , right = 0
            , left = 0
            }
        ]
        [ EH.simpleSubmodelContainer
            (1600 |> changeForMobile 400 dProfile)
            (case model.bucketSale of
                Nothing ->
                    Element.el [ Element.centerX, Element.Font.size 30 ] <| Element.text "Loading..."

                Just bucketSale ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing (20 |> changeForMobile 10 dProfile)
                        , Element.padding (20 |> changeForMobile 10 dProfile)
                        ]
                        [ testOutputElement model ]
            )
        ]
    , []
    )


testOutputElement : Model -> Element Msg
testOutputElement model =
    Element.text <|
        case model.exitInfo of
            Nothing ->
                "not loaded"

            Just exitInfo ->
                TokenValue.toConciseString exitInfo.totalExitable
                    ++ " - "
                    ++ String.fromInt (List.length exitInfo.exitableBuckets)


bucketTimestampToString : Time.Posix -> Maybe Time.Zone -> Time.Posix -> String
bucketTimestampToString now maybeTz timestamp =
    let
        tz =
            maybeTz |> Maybe.withDefault Time.utc

        timeDiff =
            TimeHelpers.secondsToPosix <|
                abs <|
                    TimeHelpers.posixToSeconds now
                        - TimeHelpers.posixToSeconds timestamp

        maybeDayString =
            let
                isSameDay =
                    (Time.toDay tz now == Time.toDay tz timestamp)
                        && (TimeHelpers.compare timeDiff TimeHelpers.oneDay == LT)

                isSameYear =
                    Time.toYear tz now == Time.toYear tz timestamp
            in
            if isSameDay then
                Nothing

            else if TimeHelpers.compare timeDiff TimeHelpers.oneWeek == LT then
                Just
                    (Time.toWeekday tz timestamp
                        |> TimeHelpers.weekdayToShortString
                    )

            else if isSameYear then
                Just <|
                    (Time.toMonth tz timestamp
                        |> TimeHelpers.monthToShortString
                    )
                        ++ " "
                        ++ (Time.toDay tz timestamp
                                |> String.fromInt
                           )

            else
                Just <|
                    (Time.toMonth tz timestamp
                        |> TimeHelpers.monthToShortString
                    )
                        ++ " "
                        ++ (Time.toDay tz timestamp
                                |> String.fromInt
                           )
                        ++ ", "
                        ++ (Time.toYear tz timestamp
                                |> String.fromInt
                           )

        timeString =
            (Time.toHour tz timestamp
                |> String.fromInt
                |> String.padLeft 2 '0'
            )
                ++ ":"
                ++ (Time.toMinute tz timestamp
                        |> String.fromInt
                        |> String.padLeft 2 '0'
                   )
    in
    (maybeDayString
        |> Maybe.map (\s -> s ++ " ")
        |> Maybe.withDefault ""
    )
        ++ timeString
        ++ (if maybeTz == Nothing then
                " (UTC)"

            else
                ""
           )
