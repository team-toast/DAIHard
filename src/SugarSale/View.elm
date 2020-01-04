module SugarSale.View exposing (root)

import BigInt exposing (BigInt)
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
            (1600 |> changeForMobile 400 dProfile)
            (case model.sugarSale of
                Nothing ->
                    Element.el [ Element.centerX, Element.Font.size 30 ] <| Element.text "Loading..."

                Just sugarSale ->
                    Element.column
                        [ Element.width Element.fill
                        , Element.spacing (20 |> changeForMobile 10 dProfile)
                        , Element.padding (20 |> changeForMobile 10 dProfile)
                        ]
                        [ viewBucketsRow sugarSale model.bucketView model.now model.testMode dProfile
                        , focusedBucketInfoElement model sugarSale dProfile
                        , focusedBucketActionElement model sugarSale model.testMode dProfile
                        ]
            )
        ]
    , []
    )


viewBucketsRow : SugarSale -> BucketView -> Time.Posix -> Bool -> DisplayProfile -> Element Msg
viewBucketsRow sugarSale bucketView now testMode dProfile =
    Element.row
        [ Element.spacing 15
        , Element.centerX
        ]
        (visibleBucketIds sugarSale bucketView now testMode
            |> List.map
                (\id ->
                    viewBucket
                        sugarSale
                        id
                        (id == getFocusedBucketId sugarSale bucketView now testMode)
                        testMode
                        dProfile
                )
        )


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
                    Element.rgb 0.8 1 0.8

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
                        case bucket.userBuy of
                            Just buy ->
                                if (not <| TokenValue.isZero buy.valueEntered) && not buy.hasExited then
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


focusedBucketInfoElement : Model -> SugarSale -> DisplayProfile -> Element Msg
focusedBucketInfoElement model sugarSale dProfile =
    let
        bucket =
            Tuple.second <|
                getBucketInfo
                    sugarSale
                    (getFocusedBucketId
                        sugarSale
                        model.bucketView
                        model.now
                        model.testMode
                    )
                    model.testMode
    in
    Element.column
        [ Element.centerX
        , Element.spacing 10
        , Element.width <| Element.px (500 |> changeForMobile 300 dProfile)
        , Element.Font.size (28 |> changeForMobile 22 dProfile)
        ]
        [ Element.row
            [ Element.width Element.fill
            ]
            [ Element.el [ Element.alignLeft ] <|
                bucketIdElement model sugarSale dProfile
            , Element.el [ Element.alignRight ] <|
                timingInfoElement model sugarSale dProfile
            ]
        , EH.thinGrayHRuler
        , Element.column
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ totalDaiBidRow bucket dProfile
            , effectiveTokenPriceRow bucket model.testMode dProfile
            ]
        ]


bucketIdElement : Model -> SugarSale -> DisplayProfile -> Element Msg
bucketIdElement model sugarSale dProfile =
    Element.column
        [ Element.spacing 5
        ]
        [ Element.text "SugarSale Bucket"
        , Element.text
            ("#"
                ++ (getFocusedBucketId sugarSale model.bucketView model.now model.testMode
                        |> String.fromInt
                   )
            )
        ]


timingInfoElement : Model -> SugarSale -> DisplayProfile -> Element Msg
timingInfoElement model sugarSale dProfile =
    let
        focusedBucketId =
            getFocusedBucketId sugarSale model.bucketView model.now model.testMode

        ( descText, timeText ) =
            case Tuple.first <| getBucketInfo sugarSale focusedBucketId model.testMode of
                Past ->
                    ( "Bucket Ended at "
                    , bucketStartTime sugarSale (focusedBucketId + 1) model.testMode
                        |> bucketTimestampToString model.now model.timezone
                    )

                Active ->
                    ( "Bucket Ends In "
                    , activeBucketTimeLeft sugarSale model.now model.testMode
                        |> TimeHelpers.toConciseIntervalString
                    )

                Future ->
                    ( "Bucket Will Start at "
                    , bucketStartTime sugarSale focusedBucketId model.testMode
                        |> bucketTimestampToString model.now model.timezone
                    )
    in
    Element.column
        [ Element.spacing 5
        ]
        [ Element.el
            [ Element.alignRight
            ]
            (Element.text descText)
        , Element.el
            [ Element.alignRight
            ]
            (Element.text timeText)
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


totalDaiBidRow : Bucket -> DisplayProfile -> Element Msg
totalDaiBidRow bucket dProfile =
    Element.row
        [ Element.width Element.fill
        ]
        [ Element.el [ Element.alignLeft ] <|
            Element.text "Total Dai Bid:"
        , Element.el [ Element.alignRight ] <|
            (bucket.totalValueEntered
                |> Maybe.map formatCalcValue
                |> Maybe.withDefault (Element.text "Loading...")
            )
        ]


effectiveTokenPriceRow : Bucket -> Bool -> DisplayProfile -> Element Msg
effectiveTokenPriceRow bucket testMode dProfile =
    Element.row
        [ Element.width Element.fill
        ]
        [ Element.el [ Element.alignLeft ] <|
            Element.text "Effective ??? Price:"
        , Element.el [ Element.alignRight ] <|
            (bucket.totalValueEntered
                |> Maybe.map
                    (\totalValueEntered ->
                        getEffectivePricePerToken totalValueEntered testMode
                    )
                |> Maybe.map formatCalcValue
                |> Maybe.withDefault (Element.text "Loading...")
            )
        ]


focusedBucketActionElement : Model -> SugarSale -> Bool -> DisplayProfile -> Element Msg
focusedBucketActionElement model sugarSale testMode dProfile =
    case Wallet.userInfo model.wallet of
        Nothing ->
            connectToWeb3Button dProfile

        Just userInfo ->
            let
                bucketId =
                    getFocusedBucketId
                        sugarSale
                        model.bucketView
                        model.now
                        model.testMode

                ( bucketState, bucket ) =
                    getBucketInfo
                        sugarSale
                        bucketId
                        model.testMode
            in
            case bucketState of
                Past ->
                    case bucket.userBuy of
                        Just buy ->
                            if (not <| TokenValue.isZero buy.valueEntered) && not buy.hasExited then
                                exitForm userInfo bucketId bucket buy model.testMode dProfile

                            else
                                noExitAvailableElement dProfile

                        Nothing ->
                            Element.text "Loading..."

                Active ->
                    entryForm userInfo bucket model.daiInput model.daiAmount model.referrer model.allowanceState model.dumbCheckboxesClicked model.testMode dProfile

                Future ->
                    Element.none


entryForm : UserInfo -> Bucket -> String -> Maybe (Result String TokenValue) -> Maybe Address -> AllowanceState -> ( Bool, Bool ) -> Bool -> DisplayProfile -> Element Msg
entryForm userInfo bucket daiInput maybeDaiInputResult maybeReferrer allowanceState dumbCheckboxesChecked testMode dProfile =
    let
        maybeInputError =
            maybeDaiInputResult
                |> Maybe.map Result.Extra.error
                |> Maybe.Extra.join
    in
    Element.column
        [ Element.width <| Element.px (800 |> changeForMobile 500 dProfile)
        , Element.centerX
        , Element.padding (25 |> changeForMobile 15 dProfile)
        , Element.spacing 20
        , Element.Background.color <| Element.rgb 0.96 0.9 0.67
        , Element.Border.color <| Element.rgb 0.9 0.85 0.4
        , Element.Border.width 1
        , Element.Border.shadow
            { offset = ( -5, 5 )
            , size = 0
            , blur = 10
            , color = Element.rgba 0 0 0 0.5
            }
        ]
        [ amountInputElement daiInput maybeInputError dProfile
        , bidConsequencesElement
            (maybeDaiInputResult
                |> Maybe.map Result.toMaybe
                |> Maybe.withDefault (Just (TokenValue.fromIntTokenValue 10))
            )
            bucket.totalValueEntered
            testMode
            dProfile
        , EH.thinGrayHRuler
        , dumbCheckboxesElement
            dumbCheckboxesChecked
            dProfile
        , maybeDepositButton
            userInfo
            allowanceState
            dumbCheckboxesChecked
            (maybeDaiInputResult
                |> Maybe.map Result.toMaybe
                |> Maybe.Extra.join
            )
            maybeReferrer
            dProfile
        ]


maybeDepositButton : UserInfo -> AllowanceState -> ( Bool, Bool ) -> Maybe TokenValue -> Maybe Address -> DisplayProfile -> Element Msg
maybeDepositButton userInfo allowanceState dumbCheckboxesChecked maybeDaiAmount maybeReferrer dProfile =
    case ( allowanceState, dumbCheckboxesChecked, maybeDaiAmount ) of
        ( Loaded allowance, ( True, True ), Just daiAmount ) ->
            if TokenValue.compare allowance daiAmount == LT then
                depositButton Nothing dProfile

            else
                depositButton
                    (Just <|
                        EnterButtonClicked
                            userInfo
                            daiAmount
                            maybeReferrer
                    )
                    dProfile

        _ ->
            depositButton Nothing dProfile


amountInputElement : String -> Maybe String -> DisplayProfile -> Element Msg
amountInputElement val maybeError dProfile =
    Element.row
        [ Element.spacing 10
        ]
        [ Element.text "Bid"
        , EH.inputContainer
            dProfile
            [ Element.Background.color <| Element.rgba 1 1 1 0.3
            , Element.Border.color <| Element.rgba 0 0 0 0.1
            , Element.width <| Element.px 70
            ]
            [ Element.Input.text
                [ Element.Border.width 0
                , Element.width Element.fill
                , Element.centerY
                , Element.Background.color <| Element.rgba 0 0 0 0
                ]
                { onChange = DaiInputChanged
                , text = val
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.color <| Element.rgba 0 0 0 0.2
                            ]
                        <|
                            Element.text "10"
                , label = Element.Input.labelHidden "dai amount in"
                }
            ]
        , Element.text "Dai"
        ]


bidConsequencesElement : Maybe TokenValue -> Maybe TokenValue -> Bool -> DisplayProfile -> Element Msg
bidConsequencesElement maybeBidAmount maybeTotalDaiAlreadyEntered testMode dProfile =
    Element.column
        [ Element.spacing 10
        , Element.width Element.fill
        , Element.Font.size (18 |> changeForMobile 16 dProfile)
        , Element.paddingXY 5 0
        , Element.height <| Element.px (250 |> changeForMobile 250 dProfile)
        ]
    <|
        case maybeTotalDaiAlreadyEntered of
            Nothing ->
                [ Element.el [ Element.centerX ] <| Element.text "Fetching Bucket State..." ]

            Just totalDaiAlreadyEntered ->
                let
                    ( maybeNewMinPrice, maybeMaxClaimableTokens ) =
                        Maybe.map
                            (\bidAmount ->
                                ( Just <| getEffectivePricePerToken (TokenValue.add totalDaiAlreadyEntered bidAmount) testMode
                                , Just <| getClaimableTokens (TokenValue.add totalDaiAlreadyEntered bidAmount) bidAmount testMode
                                )
                            )
                            maybeBidAmount
                            |> Maybe.withDefault ( Nothing, Nothing )
                in
                let
                    emphasizedText =
                        Element.el [ Element.Font.bold ] << Element.text

                    bulletedEl el =
                        Element.row
                            [ Element.width Element.fill
                            , Element.spacing 20
                            ]
                            [ Element.el
                                [ Element.alignTop
                                , Element.Font.size 28
                                ]
                              <|
                                Element.text EH.bulletPointString
                            , el
                            ]
                in
                List.map
                    (bulletedEl
                        << Element.paragraph
                            [ Element.width Element.fill ]
                    )
                    [ [ Element.text "This bid of "
                      , formatMaybeCalcValue maybeBidAmount
                      , Element.text " is irreversible, and cannot be refunded."
                      ]
                    , [ Element.text "This bid will "
                      , emphasizedText "increase"
                      , Element.text <| " the effective price per " ++ Config.sugarTokenSymbol ++ " to "
                      , formatMaybeCalcValue maybeNewMinPrice
                      , Element.text <| " DAI/" ++ Config.sugarTokenSymbol
                      ]
                    , [ emphasizedText "If no other bids are made "
                      , Element.text "before this bucket ends, you will be able to claim "
                      , formatMaybeCalcValue maybeMaxClaimableTokens
                      , Element.text <| " " ++ Config.sugarTokenSymbol ++ "."
                      ]
                    , [ emphasizedText "If other bids are made"
                      , Element.text <|
                            ", the effective price per token will increase further, and the amount of "
                                ++ Config.sugarTokenSymbol
                                ++ " you can claim from the bucket will decrease proportionally. (For example, if the total bid amount doubles, the effective price per token will also double, and your amount of claimable tokens will halve.)"
                      ]
                    ]


dumbCheckboxesElement : ( Bool, Bool ) -> DisplayProfile -> Element Msg
dumbCheckboxesElement checkedTuple dProfile =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ Element.Input.checkbox
            [ Element.width Element.fill ]
            { onChange = FirstDumbCheckboxClicked
            , icon = Element.Input.defaultCheckbox
            , checked = Tuple.first checkedTuple
            , label =
                Element.Input.labelRight
                    [ Element.width Element.fill ]
                <|
                    Element.paragraph
                        [ Element.Font.size (18 |> changeForMobile 16 dProfile)
                        , Element.width Element.fill
                        , Element.paddingXY 10 0
                        ]
                        [ Element.text <| "I understand that this bid cannot be refunded, and that if other bids are entered before the bucket ends, the amount of " ++ Config.sugarTokenSymbol ++ " I can claim will decrease."
                        ]
            }
        , Element.row
            [ Element.Font.size (18 |> changeForMobile 16 dProfile)
            ]
            [ Element.Input.checkbox
                [ Element.width Element.fill ]
                { onChange = SecondDumbCheckboxClicked
                , icon = Element.Input.defaultCheckbox
                , checked = Tuple.second checkedTuple
                , label =
                    Element.Input.labelRight
                        [ Element.width Element.fill
                        , Element.paddingEach
                            { left = 10
                            , right = 0
                            , top = 0
                            , bottom = 0
                            }
                        ]
                    <|
                        Element.text "I have read and agree to the "
                }
            , Element.newTabLink
                [ Element.Font.color EH.blue ]
                { url = "lol wut"
                , label = Element.text "terms and conditions"
                }
            , Element.text "."
            ]
        ]


formatMaybeCalcValue : Maybe TokenValue -> Element Msg
formatMaybeCalcValue =
    Maybe.map formatCalcValue
        >> Maybe.withDefault (Element.text "???")


formatCalcValue : TokenValue -> Element Msg
formatCalcValue value =
    Element.el
        [ Element.Font.color EH.blue ]
        (Element.text <|
            TokenValue.toConciseString value
        )


depositButton : Maybe Msg -> DisplayProfile -> Element Msg
depositButton maybeOnClickMsg dProfile =
    let
        buttonAttributes =
            [ Element.centerX ]
    in
    case maybeOnClickMsg of
        Just msg ->
            EH.redButton
                dProfile
                buttonAttributes
                [ "Deposit Bid" ]
                msg

        Nothing ->
            EH.disabledButton
                dProfile
                buttonAttributes
                "Deposit Bid"
                Nothing


exitForm : UserInfo -> Int -> Bucket -> Buy -> Bool -> DisplayProfile -> Element Msg
exitForm userInfo bucketId bucket buy testMode dProfile =
    case bucket.totalValueEntered of
        Just totalValueEntered ->
            let
                claimableTokens =
                    getClaimableTokens totalValueEntered buy.valueEntered testMode
            in
            Element.column
                [ Element.centerX
                , Element.spacing 10
                ]
                [ Element.paragraph
                    []
                    [ Element.text "You have "
                    , formatCalcValue claimableTokens
                    , Element.text " to claim!"
                    ]
                , exitButton userInfo bucketId dProfile
                ]

        _ ->
            Element.text "Loading..."


exitButton : UserInfo -> Int -> DisplayProfile -> Element Msg
exitButton userInfo bucketId dProfile =
    EH.redButton
        dProfile
        []
        [ "Claim Tokens" ]
        (ExitButtonClicked userInfo bucketId)


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
