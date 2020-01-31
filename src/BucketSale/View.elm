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
                    , focusedBucketPane model bucketSale
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


focusedBucketPane : Model -> BucketSale -> Element Msg
focusedBucketPane model bucketSale =
    let
        bucketId =
            getCurrentBucketId
                bucketSale
                model.now
                model.testMode
    in
    Element.column
        (commonPaneAttributes
            ++ [ Element.width <| Element.px 650
               , Element.paddingXY 35 31
               , Element.spacing 7
               ]
        )
        [ focusedBucketHeaderEl
            bucketId
            (getBucketInfo
                bucketSale
                bucketId
                model.now
                model.testMode
            )
            (Wallet.userInfo model.wallet)
            model.enterUXModel.referrer
            model.testMode
        , focusedBucketTimeLeftEl (getBucketTimeleftInfo model.now bucketSale model.bucketView)
        , enterBidUX model.enterUXModel
        ]


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
            Element.text "Upcoming Buckets"
        ]


maybeUserBalanceBlock : Wallet.State -> Maybe TokenValue -> Element Msg
maybeUserBalanceBlock wallet maybeFryBalance =
    case ( Wallet.userInfo wallet, maybeFryBalance ) of
        ( Nothing, _ ) ->
            Element.none

        ( _, Nothing ) ->
            loadingElement

        ( Just userInfo, Just fryBalance ) ->
            commonBlockContainer PassiveStyle
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
            commonBlockContainer blockStyle
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
            commonBlockContainer PassiveStyle
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


focusedBucketHeaderEl : Int -> Maybe ( BucketState, Bucket ) -> Maybe UserInfo -> Maybe Address -> Bool -> Element Msg
focusedBucketHeaderEl bucketId maybeBucketStateAndBucket maybeUserInfo maybeReferrer testMode =
    case maybeBucketStateAndBucket of
        Nothing ->
            Debug.todo "invalid shit yo"

        Just ( bucketState, bucket ) ->
            Element.column
                [ Element.spacing 8
                , Element.width Element.fill
                ]
                ([ Element.row
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
                    ++ (case ( bucket.totalValueEntered, bucket.userBuy ) of
                            ( Just totalValueEntered, Just userBuy ) ->
                                [ Element.paragraph
                                    [ Element.Font.color <| Element.rgba255 1 31 52 0.75
                                    , Element.Font.size 15
                                    ]
                                    [ emphasizedText PassiveStyle <|
                                        TokenValue.toConciseString totalValueEntered
                                    , Element.text " DAI has been bid on this bcuket so far. All bids are irreversible."
                                    ]
                                ]

                            _ ->
                                [ loadingElement ]
                       )
                )


nextBucketArrow : Int -> Element Msg
nextBucketArrow currentBucketId =
    Element.el
        [ Element.padding 4
        , Element.pointer
        , Element.Events.onClick (FocusToBucket (currentBucketId + 1))
        , Element.Font.extraBold
        ]
        (Element.text "<")


prevBucketArrow : Int -> Element Msg
prevBucketArrow currentBucketId =
    Element.el
        [ Element.padding 4
        , Element.pointer
        , Element.Events.onClick (FocusToBucket (currentBucketId - 1))
        , Element.Font.extraBold
        ]
        (Element.text ">")


referralBonusIndicator : Maybe UserInfo -> Maybe Address -> Element Msg
referralBonusIndicator maybeUserInfo maybeReferrer =
    Element.text "refferer!?"


focusedBucketTimeLeftEl : BucketTimeleftInfo -> Element Msg
focusedBucketTimeLeftEl timeleftInfo =
    case timeleftInfo of
        StartsIn timeTilStart ->
            Debug.todo ""

        StartedAndEndsIn timeLeft ->
            Debug.todo ""


enterBidUX : EnterUXModel -> Element Msg
enterBidUX enterUXModel =
    Debug.todo ""


type CommonBlockStyle
    = ActiveStyle
    | PassiveStyle


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


commonBlockContainer : CommonBlockStyle -> List (Element Msg) -> Element Msg
commonBlockContainer styleType elements =
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
        elements


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


deepBlue : Element.Color
deepBlue =
    Element.rgb255 10 33 109


deepBlueWithAlpha : Float -> Element.Color
deepBlueWithAlpha a =
    deepBlue
        |> EH.addAlpha a
