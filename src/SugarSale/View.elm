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
import Images
import List.Extra
import Routing
import SugarSale.Types exposing (..)
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
                [ viewMaybeBucketsRow model.sugarSale model.bucketView model.currentBlock 5 model.testMode dProfile
                , entryUX model dProfile
                ]
            )
        ]
    , []
    )


viewMaybeBucketsRow : Maybe SugarSale -> BucketView -> Maybe Int -> Int -> Bool -> DisplayProfile -> Element Msg
viewMaybeBucketsRow maybeSugarSale bucketView maybeCurrentBlock numBucketsToSide testMode dProfile =
    Element.el
        [ Element.centerX
        ]
    <|
        case ( maybeSugarSale, maybeCurrentBlock ) of
            ( Just sugarSale, Just currentBlock ) ->
                viewBucketsRow sugarSale bucketView currentBlock numBucketsToSide testMode dProfile

            _ ->
                Element.el [ Element.centerX ] <| Element.text "LOADING...."


viewBucketsRow : SugarSale -> BucketView -> Int -> Int -> Bool -> DisplayProfile -> Element Msg
viewBucketsRow sugarSale bucketView currentBlock numBucketsToSide testMode dProfile =
    let
        centerBucketId =
            case bucketView of
                ViewActive ->
                    getActiveBucketId sugarSale currentBlock testMode

                ViewId id ->
                    id
    in
    List.range
        (max (centerBucketId - numBucketsToSide) 0)
        (centerBucketId + numBucketsToSide)
        |> List.map
            (\id ->
                viewBucket
                    sugarSale
                    id
                    (id == centerBucketId)
                    testMode
                    dProfile
            )
        |> Element.row
            [ Element.spacing 5 ]


viewBucket : SugarSale -> Int -> Bool -> Bool -> DisplayProfile -> Element Msg
viewBucket sugarSale bucketId isFocused testMode dProfile =
    let
        ( bucketState, bucket ) =
            getBucketInfo sugarSale bucketId testMode

        containerAttributes =
            [ Element.Border.rounded 5
            , Element.padding 10
            , Element.Border.width 1
            , Element.Border.color EH.black
            , Element.alignTop
            , Element.inFront <|
                Element.el
                    [ Element.alignLeft
                    , Element.alignTop
                    , Element.Font.size 14
                    ]
                    (Element.text <| String.fromInt bucketId)
            ]
                ++ (if isFocused then
                        [ Element.width <| Element.px (100 |> changeForMobile 60 dProfile)
                        , Element.height <| Element.px (100 |> changeForMobile 60 dProfile)
                        ]

                    else
                        [ Element.width <| Element.px (80 |> changeForMobile 50 dProfile)
                        , Element.height <| Element.px (80 |> changeForMobile 50 dProfile)
                        ]
                   )
    in
    Element.el
        containerAttributes
    <|
        Element.column
            [ Element.width Element.fill
            , Element.height Element.fill
            , Element.spaceEvenly
            , Element.padding 5
            ]
            []


entryUX : Model -> DisplayProfile -> Element Msg
entryUX model dProfile =
    Element.text "entry UX"
