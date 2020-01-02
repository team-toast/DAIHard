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
import Eth.Types exposing (Address)
import FormatFloat exposing (formatFloat)
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Images
import List.Extra
import Maybe.Extra
import Result.Extra
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
                    (id == getFocusedBucketId sugarSale bucketView now testMode)
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
                focusedBucketId =
                    getFocusedBucketId
                        sugarSale
                        model.bucketView
                        model.now
                        model.testMode

                ( bucketState, bucket ) =
                    getBucketInfo
                        sugarSale
                        focusedBucketId
                        model.testMode
            in
            Element.column
                [ Element.width Element.fill
                , Element.padding 20
                ]
                [ timingInfoElement model sugarSale focusedBucketId dProfile
                , entryOrExitForm model sugarSale focusedBucketId dProfile
                ]


timingInfoElement : Model -> SugarSale -> Int -> DisplayProfile -> Element Msg
timingInfoElement model sugarSale focusedBucketId dProfile =
    Element.column
        [ Element.centerX
        , Element.Font.size (22 |> changeForMobile 18 dProfile)
        ]
    <|
        List.map
            Element.text
            (case Tuple.first <| getBucketInfo sugarSale focusedBucketId model.testMode of
                Past ->
                    [ "Bucket Ended at "
                    , bucketStartTime sugarSale (focusedBucketId + 1) model.testMode
                        |> bucketTimestampToString model.now model.timezone
                    ]

                Active ->
                    [ "Bucket Ends In "
                    , activeBucketTimeLeft sugarSale model.now model.testMode
                        |> TimeHelpers.toConciseIntervalString
                    ]

                Future ->
                    [ "Bucket Will Start at "
                    , bucketStartTime sugarSale focusedBucketId model.testMode
                        |> bucketTimestampToString model.now model.timezone
                    ]
            )


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


entryOrExitForm : Model -> SugarSale -> Int -> DisplayProfile -> Element Msg
entryOrExitForm model sugarSale bucketId dProfile =
    case Wallet.userInfo model.wallet of
        Nothing ->
            connectToWeb3Button dProfile

        Just userInfo ->
            let
                ( bucketState, bucket ) =
                    getBucketInfo sugarSale bucketId model.testMode
            in
            case bucketState of
                Past ->
                    case bucket.userExitInfo of
                        Just userExitInfo ->
                            if userExitInfo.hasTokensToClaim then
                                exitForm userInfo userExitInfo dProfile

                            else
                                noExitAvailableElement dProfile

                        Nothing ->
                            Element.text "Loading..."

                Active ->
                    entryForm userInfo bucket model.daiInput model.daiAmount model.referrer model.allowanceState model.dumbCheckboxesClicked model.testMode dProfile

                Future ->
                    Element.none


entryForm : UserInfo -> Bucket -> String -> Maybe (Result String TokenValue) -> Maybe Address -> AllowanceState -> ( Bool, Bool ) -> Bool -> DisplayProfile -> Element Msg
entryForm userInfo bucket daiInput maybeDaiInputResult maybeReferrer allowanceState dumbCheckboxesClicked testMode dProfile =
    let
        maybeInputError =
            maybeDaiInputResult
                |> Maybe.map Result.Extra.error
                |> Maybe.Extra.join
    in
    Element.column
        [ Element.width Element.fill
        , Element.spacing 10
        ]
        ([ amountInputElement daiInput maybeInputError dProfile ]
            ++ (case ( maybeDaiInputResult, bucket.totalValueEntered ) of
                    ( Just (Ok daiAmount), Just totalValueEntered ) ->
                        [ bidConsequencesElement daiAmount totalValueEntered dumbCheckboxesClicked testMode dProfile
                        , confirmButton userInfo allowanceState dumbCheckboxesClicked daiAmount maybeReferrer dProfile
                        ]

                    _ ->
                        []
               )
        )


amountInputElement : String -> Maybe String -> DisplayProfile -> Element Msg
amountInputElement val maybeError dProfile =
    Element.row
        [ Element.spacing 10
        , Element.centerX
        ]
        [ Element.text "Bid"
        , EH.inputContainer
            dProfile
            []
            [ Element.Input.text
                [ Element.Border.width 0
                , Element.width <| Element.px 100
                , Element.height Element.fill
                ]
                { onChange = DaiInputChanged
                , text = val
                , placeholder = Nothing
                , label = Element.Input.labelHidden "dai amount in"
                }
            ]
        , Element.text "Dai"
        ]


bidConsequencesElement : TokenValue -> TokenValue -> ( Bool, Bool ) -> Bool -> DisplayProfile -> Element Msg
bidConsequencesElement bidAmount totalDaiAlreadyEntered dumbCheckboxesClicked testMode dProfile =
    let
        newMinPrice =
            getEffectivePricePerToken (TokenValue.add totalDaiAlreadyEntered bidAmount) testMode

        maxClaimableTokens =
            getClaimableTokens (TokenValue.add totalDaiAlreadyEntered bidAmount) bidAmount testMode
    in
    Element.column
        [ Element.centerX
        , Element.padding 10
        ]
    <|
        [ Element.row
            [ Element.width Element.fill
            , Element.spacing 15
            ]
            [ Element.text "Min ??? price for this bucket becomes"
            , formatCalcValue newMinPrice
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 15
            ]
            [ Element.text "If no one else enters in ??? you get"
            , formatCalcValue maxClaimableTokens
            ]
        , Element.Input.checkbox
            []
            { onChange = FirstDumbCheckboxClicked
            , icon = Element.Input.defaultCheckbox
            , checked = Tuple.first dumbCheckboxesClicked
            , label = Element.Input.labelRight [] <| Element.text "I understand that this bid is irreversible, and that if others bid I will claim less than ???"
            }
        , Element.Input.checkbox
            []
            { onChange = SecondDumbCheckboxClicked
            , icon = Element.Input.defaultCheckbox
            , checked = Tuple.second dumbCheckboxesClicked
            , label = Element.Input.labelRight [] <| Element.text "I have read and agree to the terms and conditions"
            }
        ]


formatCalcValue : TokenValue -> Element Msg
formatCalcValue value =
    Element.el
        [ Element.Font.color EH.blue ]
        (Element.text <|
            TokenValue.toConciseString value
        )


confirmButton : UserInfo -> AllowanceState -> ( Bool, Bool ) -> TokenValue -> Maybe Address -> DisplayProfile -> Element Msg
confirmButton userInfo allowanceState dumbCheckboxesClicked daiAmount maybeReferrer dProfile =
    let
        buttonAttributes =
            [ Element.centerX ]
    in
    case allowanceState of
        Loading ->
            EH.disabledButton
                dProfile
                buttonAttributes
                "Loading Dai account..."
                Nothing

        UnlockMining ->
            EH.disabledButton
                dProfile
                buttonAttributes
                "Unlocking Dai..."
                Nothing

        Loaded allowance ->
            if TokenValue.compare allowance daiAmount == LT then
                EH.redButton
                    dProfile
                    buttonAttributes
                    [ "Unlock Dai" ]
                    UnlockDaiButtonClicked

            else
                let
                    enabled =
                        Tuple.first dumbCheckboxesClicked && Tuple.second dumbCheckboxesClicked
                in
                if enabled then
                    EH.redButton
                        dProfile
                        buttonAttributes
                        [ "Deposit This Bid" ]
                        (EnterButtonClicked userInfo daiAmount maybeReferrer)

                else
                    EH.disabledButton
                        dProfile
                        buttonAttributes
                        "Deposit This Bid"
                        Nothing


exitForm : UserInfo -> BucketUserExitInfo -> DisplayProfile -> Element Msg
exitForm userInfo userExitInfo dProfile =
    Debug.todo ""


noExitAvailableElement : DisplayProfile -> Element Msg
noExitAvailableElement dProfile =
    Element.el
        [ Element.centerX
        , Element.Font.size (18 |> changeForMobile 14 dProfile)
        , Element.Font.bold
        , Element.Font.italic
        , Element.Font.color EH.darkGray
        ]
        (Element.text "You have no claimable ??? in this bucket.")


connectToWeb3Button : DisplayProfile -> Element Msg
connectToWeb3Button dProfile =
    Element.el
        [ Element.centerX
        , Element.padding (17 |> changeForMobile 10 dProfile)
        , Element.Border.rounded 4
        , Element.Font.size (20 |> changeForMobile 16 dProfile)
        , Element.Font.semiBold
        , Element.Font.center
        , Element.Background.color EH.softRed
        , Element.Font.color EH.white
        , Element.pointer
        , Element.Events.onClick <| CmdUp CmdUp.Web3Connect
        ]
        (Element.text "Connect to Wallet")
