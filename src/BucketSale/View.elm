module BucketSale.View exposing (root)

import BigInt exposing (BigInt)
import BucketSale.Types exposing (..)
import CmdUp exposing (CmdUp)
import CommonTypes exposing (..)
import Config
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
                    , focusedBucketPane model
                    , futureBucketsPane model
                    ]
        ]
    , []
    )


commonPaneAttributes : List (Attribute Msg)
commonPaneAttributes =
    [ Element.Background.color EH.white
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
               , Element.spacing 7
               ]
        )
        [ Element.el
            [ Element.Font.size 25
            , Element.Font.bold
            ]
          <|
            Element.text "Previous Buckets"
        , Element.paragraph
            [ Element.Font.color <| Element.rgba255 1 31 52 0.75
            , Element.Font.size 15
            ]
            [ Element.text "These are the previous buckets of FRY that have been claimed. If you have FRY to claim it will show below." ]
        , maybeUserBalanceBlock model.wallet model.userFryBalance
        ]


focusedBucketPane : Model -> Element Msg
focusedBucketPane model =
    Element.column
        (commonPaneAttributes
            ++ [ Element.width <| Element.px 650
               , Element.paddingXY 35 31
               , Element.spacing 7
               ]
        )
        [ Element.el
            [ Element.Font.size 30
            , Element.Font.bold
            ]
          <|
            Element.text <|
                "Current bucket of "
                    ++ TokenValue.toConciseString (Config.bucketSaleTokensPerBucket model.testMode)
                    ++ " "
                    ++ Config.bucketTokenSymbol
        , Element.el
            [ Element.height <| Element.px 600
            ]
          <|
            Element.text "hi"
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
         , Element.spacing 3
         , Element.Font.color <| deepBlueWithAlpha 0.3
         ]
            ++ (case styleType of
                    ActiveStyle ->
                        [ Element.Background.color deepBlue ]

                    PassiveStyle ->
                        [ Element.Background.color <| deepBlueWithAlpha 0.05 ]
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
