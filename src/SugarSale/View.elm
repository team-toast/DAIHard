module SugarSale.View exposing (root)

import BigInt exposing (BigInt)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Element exposing (Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FormatFloat exposing (formatFloat)
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Images
import List.Extra
import Routing
import SugarSale.Types exposing (..)
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
            (1200 |> changeForMobile 400 dProfile)
            (Element.column
                [ Element.width Element.fill
                , Element.spacing (20 |> changeForMobile 10 dProfile)
                , Element.padding (20 |> changeForMobile 10 dProfile)
                ]
                [ viewMaybeBucketsRow model.sugarSale model.bucketView model.now model.testMode dProfile
                , entryUX model dProfile
                ]
            )
        ]
    , []
    )


viewMaybeBucketsRow : Maybe SugarSale -> BucketView -> Time.Posix -> Bool -> DisplayProfile -> Element Msg
viewMaybeBucketsRow maybeSugarSale bucketView now testMode dProfile =
    Element.el
        [ Element.centerX
        ]
    <|
        case maybeSugarSale of
            Just sugarSale ->
                viewBucketsRow sugarSale bucketView now testMode dProfile

            _ ->
                Element.el [ Element.centerX ] <| Element.text "LOADING...."


viewBucketsRow : SugarSale -> BucketView -> Time.Posix -> Bool -> DisplayProfile -> Element Msg
viewBucketsRow sugarSale bucketView now testMode dProfile =
    visibleBucketIds sugarSale bucketView now testMode
        |> List.map
            (\id ->
                viewBucket
                    sugarSale
                    id
                    (id == focusedBucketId sugarSale bucketView now testMode)
                    testMode
                    dProfile
            )
        |> Element.row
            [ Element.spacing 15 ]


viewBucket : SugarSale -> Int -> Bool -> Bool -> DisplayProfile -> Element Msg
viewBucket sugarSale bucketId isFocused testMode dProfile =
    let
        ( bucketState, bucket ) =
            getBucketInfo sugarSale bucketId testMode

        borderColor =
            Element.rgb 0.8 0.8 0.8

        backgroundColor =
            case bucketState of
                Past ->
                    Element.rgba 0 1 0 0.2

                Active ->
                    EH.lightBlue

                Future ->
                    EH.white

        containerAttributes =
            [ Element.Border.rounded 5
            , Element.padding 10
            , Element.Border.width 1
            , Element.Border.color borderColor
            , Element.Background.color backgroundColor
            , Element.alignTop
            , Element.Events.onClick (BucketClicked bucketId)
            , Element.pointer
            , Element.inFront <|
                Element.el
                    [ Element.alignLeft
                    , Element.alignTop
                    , Element.Font.size 14
                    , Element.Background.color borderColor
                    , Element.padding 2
                    ]
                    (Element.text <| String.fromInt bucketId)
            ]
                ++ (if isFocused then
                        [ Element.width <| Element.px (200 |> changeForMobile 120 dProfile)
                        , Element.height <| Element.px (200 |> changeForMobile 120 dProfile)
                        ]

                    else
                        [ Element.width <| Element.px (170 |> changeForMobile 80 dProfile)
                        , Element.height <| Element.px (170 |> changeForMobile 80 dProfile)
                        ]
                   )
                ++ (if bucketState /= Future then
                        [ Element.Border.shadow
                            { offset = ( -5, 5 )
                            , size = 0
                            , blur = 5
                            , color = Element.rgba 0 0 0 0.2
                            }
                        ]

                    else
                        [ Element.Border.dashed ]
                   )
    in
    Element.el
        containerAttributes
    <|
        if bucketState == Active || bucketState == Past then
            let
                actionMsg =
                    if bucketState == Active then
                        Element.text "Can enter!"

                    else
                        case bucket.userExitInfo of
                            Just exitInfo ->
                                if exitInfo.hasTokensToClaim then
                                    Element.text "Can exit!"

                                else
                                    Element.none

                            Nothing ->
                                Element.text "Loading..."
            in
            Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spaceEvenly
                , Element.padding 5
                ]
                [ Maybe.map
                    (\totalValueEntered ->
                        Element.text <|
                            TokenValue.toConciseString totalValueEntered
                                ++ " Dai Entered"
                    )
                    bucket.totalValueEntered
                    |> Maybe.withDefault (Element.text "Loading...")
                , actionMsg
                ]

        else
            Element.text "lol XD"


entryUX : Model -> DisplayProfile -> Element Msg
entryUX model dProfile =
    case model.sugarSale of
        Nothing ->
            Element.text "Loading..."

        Just sugarSale ->
            let
                id =
                    focusedBucketId
                        sugarSale
                        model.bucketView
                        model.now
                        model.testMode

                ( bucketState, bucket ) =
                    getBucketInfo
                        sugarSale
                        id
                        model.testMode

                timingMsgLines =
                    case bucketState of
                        Past ->
                            [ "Bucket Ended at "
                            , bucketStartTime sugarSale (id + 1) model.testMode
                                |> bucketTimestampToString model.now model.timezone
                            ]

                        Active ->
                            [ "Bucket Ends In "
                            , activeBucketTimeLeft sugarSale model.now model.testMode
                                |> TimeHelpers.toConciseIntervalString
                            ]

                        Future ->
                            [ "Bucket Will Start at "
                            , bucketStartTime sugarSale id model.testMode
                                |> bucketTimestampToString model.now model.timezone
                            ]
            in
            Element.column
                [ Element.width Element.fill
                , Element.padding 20
                ]
                [ Element.column
                    [ Element.centerX
                    , Element.Font.size (22 |> changeForMobile 18 dProfile)
                    ]
                  <|
                    List.map
                        Element.text
                        timingMsgLines
                ]


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
