module BucketSale.View exposing (root)

import BigInt exposing (BigInt)
import BucketSale.Types exposing (..)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
import Contracts.BucketSale.Wrappers exposing (ExitInfo)
import Element exposing (Attribute, Element)
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


root : Model -> ( Element Msg, List (Element Msg) )
root model =
    ( Element.column
        [ Element.width Element.fill
        , Element.paddingEach
            { bottom = 40
            , top = 0
            , right = 0
            , left = 0
            }
        ]
        [ case model.bucketSale of
            Nothing ->
                Element.el [ Element.centerX, Element.Font.size 30 ] <| Element.text "Loading..."

            Just bucketSale ->
                Element.row
                    [ Element.centerX
                    , Element.spacing 50
                    ]
                    [ closedBucketsPane model
                    , focusedBucketPane
                        bucketSale
                        (getFocusedBucketId
                            bucketSale
                            model.bucketView
                            model.now
                            model.testMode
                        )
                        model.wallet
                        model.enterUXModel
                        model.now
                        model.testMode
                    , futureBucketsPane model
                    ]
        ]
    , []
    )


commonPaneAttributes : List (Attribute Msg)
commonPaneAttributes =
    [ Element.Background.color EH.white
    , Element.spacing 20
    , Element.Border.rounded 8
    , Element.centerY
    , Element.Border.shadow
        { offset = ( 0, 3 )
        , size = 0
        , blur = 20
        , color = Element.rgba 0 0 0 0.06
        }
    ]


closedBucketsPane : Model -> Element Msg
closedBucketsPane model =
    Element.column
        (commonPaneAttributes
            ++ [ Element.width <| Element.px 430
               , Element.paddingXY 32 25
               ]
        )
        [ Element.el
            [ Element.Font.size 25
            , Element.Font.bold
            ]
          <|
            Element.text "Concluded Buckets"
        , Element.paragraph
            [ Element.Font.color <| Element.rgba255 1 31 52 0.75
            , Element.Font.size 15
            ]
            [ Element.text "These are the concluded buckets of FRY that have been claimed. If you have FRY to claim it will show below." ]
        , maybeUserBalanceBlock model.wallet model.userFryBalance
        , maybeClaimBlock model.wallet model.exitInfo
        , totalExitedBlock model.totalTokensExited
        ]


focusedBucketPane : BucketSale -> Int -> Wallet.State -> EnterUXModel -> Time.Posix -> Bool -> Element Msg
focusedBucketPane bucketSale bucketId wallet enterUXModel now testMode =
    Element.column
        (commonPaneAttributes
            ++ [ Element.width <| Element.px 650
               , Element.paddingXY 35 31
               , Element.spacing 7
               , Element.height <| Element.px 800
               ]
        )
        ([ focusedBucketHeaderEl
            bucketId
            (Wallet.userInfo wallet)
            enterUXModel.referrer
            testMode
         ]
            ++ (case getBucketInfo bucketSale bucketId now testMode of
                    InvalidBucket ->
                        [ Element.el
                            [ Element.Font.size 20
                            , Element.centerX
                            ]
                            (Element.text "Invalid bucket Id")
                        ]

                    ValidBucket bucketInfo ->
                        [ focusedBucketSubheaderEl bucketInfo
                        , focusedBucketTimeLeftEl
                            (getRelevantTimingInfo bucketInfo now testMode)
                            testMode
                        , enterBidUX enterUXModel bucketInfo testMode
                        ]
               )
        )


futureBucketsPane : Model -> Element Msg
futureBucketsPane model =
    Element.column
        (commonPaneAttributes
            ++ [ Element.paddingXY 32 25
               , Element.spacing 7
               ]
        )
        [ Element.el
            [ Element.width <| Element.px 430
            , Element.Font.size 25
            , Element.Font.bold
            ]
          <|
            Element.text "Future Buckets"
        ]


maybeUserBalanceBlock : Wallet.State -> Maybe TokenValue -> Element Msg
maybeUserBalanceBlock wallet maybeFryBalance =
    case ( Wallet.userInfo wallet, maybeFryBalance ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, Nothing ) ->
            loadingElement

        ( Just userInfo, Just fryBalance ) ->
            sidepaneBlockContainer PassiveStyle
                [ bigNumberElement
                    [ Element.centerX ]
                    (TokenNum fryBalance)
                    Config.bucketTokenSymbol
                    PassiveStyle
                , Element.paragraph
                    [ Element.centerX
                    , Element.width Element.shrink
                    ]
                    [ Element.text "in your wallet"
                    ]
                ]


maybeClaimBlock : Wallet.State -> Maybe ExitInfo -> Element Msg
maybeClaimBlock wallet maybeExitInfo =
    case ( Wallet.userInfo wallet, maybeExitInfo ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, Nothing ) ->
            loadingElement

        ( Just userInfo, Just exitInfo ) ->
            let
                ( blockStyle, maybeClaimButton ) =
                    if TokenValue.isZero exitInfo.totalExitable then
                        ( PassiveStyle, Nothing )

                    else
                        ( ActiveStyle, Just <| makeClaimButton userInfo exitInfo )
            in
            sidepaneBlockContainer blockStyle
                [ bigNumberElement
                    [ Element.centerX ]
                    (TokenNum exitInfo.totalExitable)
                    Config.bucketTokenSymbol
                    blockStyle
                , Element.paragraph
                    [ Element.centerX
                    , Element.width Element.shrink
                    ]
                    [ Element.text "available for "
                    , emphasizedText blockStyle "you"
                    , Element.text " to claim"
                    ]
                , Maybe.map
                    (Element.el [ Element.centerX ])
                    maybeClaimButton
                    |> Maybe.withDefault Element.none
                ]


totalExitedBlock : Maybe TokenValue -> Element Msg
totalExitedBlock maybeTotalExited =
    case maybeTotalExited of
        Nothing ->
            loadingElement

        Just totalExited ->
            sidepaneBlockContainer PassiveStyle
                [ bigNumberElement
                    [ Element.centerX ]
                    (TokenNum totalExited)
                    Config.bucketTokenSymbol
                    PassiveStyle
                , Element.paragraph
                    [ Element.centerX
                    , Element.width Element.shrink
                    ]
                    [ Element.text "claimed by "
                    , emphasizedText PassiveStyle "others"
                    ]
                ]


focusedBucketHeaderEl : Int -> Maybe UserInfo -> Maybe Address -> Bool -> Element Msg
focusedBucketHeaderEl bucketId maybeUserInfo maybeReferrer testMode =
    Element.column
        [ Element.spacing 8
        , Element.width Element.fill
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ Element.row
                [ Element.Font.size 30
                , Element.Font.bold
                , Element.alignLeft
                , Element.spacing 10
                ]
                [ prevBucketArrow bucketId
                , Element.text <|
                    "Bucket #"
                        ++ String.fromInt bucketId
                , nextBucketArrow bucketId
                ]
            , Element.el
                [ Element.alignRight ]
              <|
                referralBonusIndicator maybeUserInfo maybeReferrer
            ]
        ]


focusedBucketSubheaderEl : ValidBucketInfo -> Element Msg
focusedBucketSubheaderEl bucketInfo =
    case ( bucketInfo.bucketData.totalValueEntered, bucketInfo.bucketData.userBuy ) of
        ( Just totalValueEntered, Just userBuy ) ->
            Element.paragraph
                [ Element.Font.color <| Element.rgba255 1 31 52 0.75
                , Element.Font.size 15
                ]
                [ emphasizedText PassiveStyle <|
                    TokenValue.toConciseString totalValueEntered
                , Element.text " DAI has been bid on this bcuket so far. All bids are irreversible."
                ]

        _ ->
            loadingElement


nextBucketArrow : Int -> Element Msg
nextBucketArrow currentBucketId =
    Element.el
        [ Element.padding 4
        , Element.pointer
        , Element.Events.onClick (FocusToBucket (currentBucketId + 1))
        , Element.Font.extraBold
        ]
        (Element.text ">")


prevBucketArrow : Int -> Element Msg
prevBucketArrow currentBucketId =
    Element.el
        [ Element.padding 4
        , Element.pointer
        , Element.Events.onClick (FocusToBucket (currentBucketId - 1))
        , Element.Font.extraBold
        ]
        (Element.text "<")


referralBonusIndicator : Maybe UserInfo -> Maybe Address -> Element Msg
referralBonusIndicator maybeUserInfo maybeReferrer =
    Element.text "refferer!?"


focusedBucketTimeLeftEl : RelevantTimingInfo -> Bool -> Element Msg
focusedBucketTimeLeftEl timingInfo testMode =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 22
        ]
        [ progressBarElement
            (case timingInfo.state of
                Current ->
                    Just <|
                        1
                            - ((Time.posixToMillis timingInfo.relevantTimeFromNow |> toFloat)
                                / (Time.posixToMillis (Config.bucketSaleBucketInterval testMode) |> toFloat)
                              )

                _ ->
                    Nothing
            )
        , let
            intervalString =
                TimeHelpers.toConciseIntervalString timingInfo.relevantTimeFromNow
          in
          (Element.el
            [ Element.Font.color deepBlue ]
            << Element.text
          )
            (case timingInfo.state of
                Closed ->
                    "ended " ++ intervalString ++ " ago"

                Current ->
                    intervalString ++ " left"

                Future ->
                    "starts in " ++ intervalString
            )
        ]


enterBidUX : EnterUXModel -> ValidBucketInfo -> Bool -> Element Msg
enterBidUX enterUXModel bucketInfo testMode =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 20
        ]
        [ bidInputBlock enterUXModel bucketInfo testMode
        , bidImpactBlock enterUXModel bucketInfo testMode
        , otherBidsImpactMsg
        , continueButton enterUXModel bucketInfo
        ]


bidInputBlock : EnterUXModel -> ValidBucketInfo -> Bool -> Element Msg
bidInputBlock enterUXModel bucketInfo testMode =
    centerpaneBlockContainer ActiveStyle
        [ emphasizedText ActiveStyle "I want to bid:"
        , Element.row
            [ Element.Background.color <| Element.rgba 1 1 1 0.08
            , Element.Border.rounded 4
            , Element.padding 13
            , Element.width Element.fill
            ]
            [ Element.Input.text
                [ Element.Font.size 19
                , Element.Font.medium
                , Element.Font.color EH.white
                , Element.Border.width 0
                , Element.width Element.fill
                , Element.Background.color EH.transparent
                ]
                { onChange = DaiInputChanged
                , text = enterUXModel.daiInput
                , placeholder =
                    Just <|
                        Element.Input.placeholder
                            [ Element.Font.medium
                            , Element.Font.color <| Element.rgba 1 1 1 0.25
                            ]
                            (Element.text "Enter Amount")
                , label = Element.Input.labelHidden "bid amount"
                }
            , Element.row
                [ Element.centerY
                , Element.spacing 10
                ]
                [ Images.daiSymbol
                    |> Images.toElement [ Element.height <| Element.px 30 ]
                , Element.text "DAI"
                ]
            ]
        , Maybe.map
            (\totalValueEntered ->
                pricePerTokenMsg
                    totalValueEntered
                    (enterUXModel.daiAmount
                        |> Maybe.map Result.toMaybe
                        |> Maybe.Extra.join
                    )
                    testMode
            )
            bucketInfo.bucketData.totalValueEntered
            |> Maybe.withDefault loadingElement
        ]


pricePerTokenMsg : TokenValue -> Maybe TokenValue -> Bool -> Element Msg
pricePerTokenMsg totalValueEntered maybeDaiAmount testMode =
    Element.paragraph
        [ Element.Font.size 14
        , Element.Font.medium
        ]
        ([ Element.text <|
            "The current FRY price is "
                ++ (calcEffectivePricePerToken
                        totalValueEntered
                        testMode
                        |> TokenValue.toConciseString
                   )
                ++ " DAI/FRY."
         ]
            ++ (case maybeDaiAmount of
                    Just amount ->
                        [ Element.text " This bid will increase the price to "
                        , emphasizedText ActiveStyle <|
                            (calcEffectivePricePerToken
                                (TokenValue.add
                                    totalValueEntered
                                    amount
                                )
                                testMode
                                |> TokenValue.toConciseString
                            )
                                ++ " DAI/FRY."
                        ]

                    _ ->
                        []
               )
        )


bidImpactBlock : EnterUXModel -> ValidBucketInfo -> Bool -> Element Msg
bidImpactBlock enterUXModel bucketInfo testMode =
    centerpaneBlockContainer PassiveStyle
        [ emphasizedText PassiveStyle "Your current bid standing:"
        , case Debug.log "impact" ( bucketInfo.bucketData.totalValueEntered, bucketInfo.bucketData.userBuy ) of
            ( Just totalValueEntered, Just userBuy ) ->
                let
                    existingBidAmount =
                        userBuy.valueEntered

                    extraBidAmount =
                        enterUXModel.daiAmount
                            |> Maybe.map Result.toMaybe
                            |> Maybe.Extra.join
                            |> Maybe.withDefault TokenValue.zero

                    totalBidAmount =
                        TokenValue.add
                            extraBidAmount
                            existingBidAmount
                in
                Element.paragraph
                    [ Element.width Element.fill
                    , Element.Font.color <| Element.rgba255 1 31 52 0.75
                    ]
                <|
                    if TokenValue.isZero totalBidAmount then
                        [ Element.text "You haven't entered any bids into this bucket." ]

                    else
                        [ Element.text "If no other bids are made before this bucket ends, you will be able to claim "
                        , emphasizedText PassiveStyle <|
                            (calcClaimableTokens totalValueEntered totalBidAmount testMode
                                |> TokenValue.toConciseString
                            )
                                ++ " FRY"
                        , Element.text <|
                            " out of "
                                ++ TokenValue.toConciseString (Config.bucketSaleTokensPerBucket testMode)
                                ++ " FRY available from the "
                        , emphasizedText PassiveStyle <|
                            TokenValue.toConciseString totalBidAmount
                                ++ " DAI"
                        , Element.text " total you'll have bid."
                        ]

            _ ->
                loadingElement
        ]


otherBidsImpactMsg : Element Msg
otherBidsImpactMsg =
    centerpaneBlockContainer PassiveStyle
        [ emphasizedText PassiveStyle "If other bids ARE made:" ]


continueButton : EnterUXModel -> ValidBucketInfo -> Element Msg
continueButton enterUXModel bucketInfo =
    Element.text "Continue button!"


progressBarElement : Maybe Float -> Element Msg
progressBarElement maybeRatioComplete =
    let
        commonStyles =
            [ Element.Border.rounded 4
            , Element.height <| Element.px 8
            ]
    in
    Element.row
        (commonStyles
            ++ [ Element.width Element.fill
               , Element.Background.color <| Element.rgba255 235 237 243 0.6
               ]
        )
        (case maybeRatioComplete of
            Just ratioComplete ->
                [ Element.el
                    (commonStyles
                        ++ [ Element.width <| Element.fillPortion (ratioComplete * 200 |> floor)
                           , Element.Background.color <| Element.rgb255 255 0 120
                           ]
                    )
                    Element.none
                , Element.el
                    [ Element.width <| Element.fillPortion ((1 - ratioComplete) * 200 |> floor) ]
                    Element.none
                ]

            Nothing ->
                []
        )


emphasizedText : CommonBlockStyle -> (String -> Element Msg)
emphasizedText styleType =
    Element.el
        (case styleType of
            ActiveStyle ->
                [ Element.Font.color EH.white ]

            PassiveStyle ->
                [ Element.Font.color deepBlue ]
        )
        << Element.text


type CommonBlockStyle
    = ActiveStyle
    | PassiveStyle


centerpaneBlockContainer : CommonBlockStyle -> List (Element Msg) -> Element Msg
centerpaneBlockContainer styleType =
    Element.column
        ([ Element.width Element.fill
         , Element.Border.rounded 4
         , Element.padding 20
         , Element.spacing 13
         , Element.Font.size 16
         ]
            ++ (case styleType of
                    ActiveStyle ->
                        [ Element.Background.color deepBlue
                        , Element.Font.color <| Element.rgba 1 1 1 0.6
                        ]

                    PassiveStyle ->
                        [ Element.Background.color gray ]
               )
        )


sidepaneBlockContainer : CommonBlockStyle -> List (Element Msg) -> Element Msg
sidepaneBlockContainer styleType =
    Element.column
        ([ Element.width Element.fill
         , Element.Border.rounded 4
         , Element.paddingXY 22 18
         , Element.spacing 16
         ]
            ++ (case styleType of
                    ActiveStyle ->
                        [ Element.Background.color deepBlue
                        , Element.Font.color <| Element.rgba 1 1 1 0.6
                        ]

                    PassiveStyle ->
                        [ Element.Background.color <| deepBlueWithAlpha 0.05
                        , Element.Font.color <| deepBlueWithAlpha 0.3
                        ]
               )
        )


type NumberVal
    = IntegerNum Int
    | TokenNum TokenValue


numberValToString : NumberVal -> String
numberValToString numberVal =
    case numberVal of
        IntegerNum intVal ->
            formatFloat 0 (toFloat intVal)

        TokenNum tokenValue ->
            TokenValue.toConciseString tokenValue


bigNumberElement : List (Attribute Msg) -> NumberVal -> String -> CommonBlockStyle -> Element Msg
bigNumberElement attributes numberVal numberLabel blockStyle =
    Element.el
        (attributes
            ++ [ Element.Font.size 27
               , Element.Font.bold
               , Element.Font.color
                    (case blockStyle of
                        ActiveStyle ->
                            EH.white

                        PassiveStyle ->
                            deepBlue
                    )
               ]
        )
        (Element.text
            (numberValToString numberVal
                ++ " "
                ++ numberLabel
            )
        )


makeClaimButton : UserInfo -> ExitInfo -> Element Msg
makeClaimButton userInfo exitInfo =
    EH.lightBlueButton
        Desktop
        [ Element.width Element.fill ]
        [ "Claim your FRY" ]
        (ClaimClicked userInfo exitInfo)


loadingElement : Element Msg
loadingElement =
    Element.text "Loading"


gray : Element.Color
gray =
    Element.rgb255 235 237 243


deepBlue : Element.Color
deepBlue =
    Element.rgb255 10 33 109


deepBlueWithAlpha : Float -> Element.Color
deepBlueWithAlpha a =
    deepBlue
        |> EH.addAlpha a
