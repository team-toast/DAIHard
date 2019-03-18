module Search.View exposing (root)

import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue exposing (FiatValue)
import Html.Events.Extra
import PaymentMethods exposing (PaymentMethod)
import Search.Types exposing (..)
import Time
import TimeHelpers
import Utils


root : Time.Posix -> Model -> Element Msg
root time model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ searchInputElement model.inputs model.searchTerms
        , EH.hbreak
        , resultsElement time model
        ]


searchInputElement : SearchInputs -> List String -> Element Msg
searchInputElement inputs searchTerms =
    Element.column [ Element.spacing 10, Element.width Element.fill, Element.padding 30 ]
        [ Element.row
            [ Element.width Element.fill
            , Element.height <| Element.px 100
            , Element.spacing 10
            ]
            [ Element.el [ Element.width <| Element.fillPortion 3 ] <|
                daiRangeInput (AmountRange Nothing Nothing)
            , Element.el [ Element.width <| Element.fillPortion 3 ] <|
                fiatRangeInput (AmountRange Nothing Nothing)
            , Element.el [ Element.width <| Element.fillPortion 6 ] <|
                paymentMethodsInput inputs.paymentMethod
            , Element.el [] <|
                resetButton
            ]
        , Element.row
            [ Element.width Element.fill
            , Element.spacing 10
            ]
            [ Element.el [ Element.width <| Element.fillPortion 6 ] Element.none
            , Element.el [ Element.width <| Element.fillPortion 6 ] <| searchTermsDisplayElement searchTerms
            ]
        ]


searchTermsDisplayElement : List String -> Element Msg
searchTermsDisplayElement searchTerms =
    case searchTerms of
        [] ->
            Element.none

        terms ->
            Element.row [ Element.width Element.fill, Element.padding 10, Element.spacing 10 ]
                (terms
                    |> List.map
                        (\term ->
                            Element.el
                                [ Element.Background.color <| Element.rgba255 16 7 234 0.2
                                , Element.Border.rounded 5
                                , Element.padding 4
                                ]
                                (Element.text term)
                        )
                )


resultsElement : Time.Posix -> Model -> Element Msg
resultsElement time model =
    let
        visibleTrades =
            model.trades
                |> Array.toList
                |> getLoadedTrades
                |> filterAndSortTrades time model.filterFunc model.sortFunc

        buyingOrSellingString =
            case model.openMode of
                Contracts.Types.BuyerOpened ->
                    "Buying"

                Contracts.Types.SellerOpened ->
                    "Selling"
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ cellMaker ( 2, sortableColumnHeader "Offer Expires" Expiring Nothing )
            , cellMaker ( 2, sortableColumnHeader buyingOrSellingString TradeAmount Nothing )
            , cellMaker ( 2, sortableColumnHeader "For Fiat" Fiat Nothing )
            , cellMaker ( 1, sortableColumnHeader "Margin" Margin Nothing )
            , cellMaker ( 6, columnHeader "Accepted Payment Methods" )
            , cellMaker ( 2, sortableColumnHeader "Payment Window" AutoabortWindow Nothing )
            , cellMaker ( 2, sortableColumnHeader "Auto-Release" AutoreleaseWindow Nothing )
            , cellMaker ( 2, Element.none )
            ]
        , Element.column
            [ Element.width Element.fill
            , Element.Border.width 1
            , Element.Border.rounded 8
            , Element.Border.color EH.lightGray
            , Element.spacing 1
            , Element.Background.color EH.lightGray
            , Element.clip
            ]
            (visibleTrades
                |> List.map
                    (viewTradeRow time)
            )
        ]


daiRangeInput : AmountRange -> Element Msg
daiRangeInput range =
    EH.textInputWithElement [] (Element.text "hi") "dairange" "" Nothing (\_ -> NoOp)
        |> withInputHeader "Dai Amount"


fiatTypeInput : Maybe FiatValue -> Element Msg
fiatTypeInput fiatType =
    dummyTextInput
        |> withInputHeader "Fiat Type"


fiatRangeInput : AmountRange -> Element Msg
fiatRangeInput range =
    dummyTextInput
        |> withInputHeader "Fiat Amount"


paymentMethodsInput : String -> Element Msg
paymentMethodsInput searchString =
    Element.Input.text
        [ Element.width Element.fill
        , Element.height <| Element.px 40
        , Element.Border.color EH.lightGray
        , Element.Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 20
            , color = Element.rgba255 233 237 242 0.05
            }
        , Element.htmlAttribute <| Html.Events.Extra.onEnter AddSearchTerm
        ]
        { onChange = SearchInputChanged
        , text = searchString
        , placeholder = Nothing
        , label = Element.Input.labelHidden "payment methods search"
        }
        |> withInputHeader "Payment Methods"


dummyTextInput =
    Element.Input.text
        [ Element.width Element.fill
        , Element.height <| Element.px 40
        , Element.Border.color EH.lightGray
        , Element.Border.shadow
            { offset = ( 0, 3 )
            , size = 0
            , blur = 20
            , color = Element.rgba255 233 237 242 0.05
            }
        ]
        { onChange = \_ -> NoOp
        , text = ""
        , placeholder = Nothing
        , label = Element.Input.labelHidden ""
        }


resetButton : Element Msg
resetButton =
    Element.Input.button
        [ Element.Background.color EH.buttonDeepBlue
        , Element.padding 10
        , Element.Border.rounded 5
        ]
        { onPress = Just ResetSearch
        , label =
            Element.el
                [ Element.Font.color EH.white
                , Element.centerX
                , Element.centerY
                ]
                (Element.text "Reset")
        }
        |> withInputHeader " "


withInputHeader : String -> Element Msg -> Element Msg
withInputHeader title element =
    Element.column
        [ Element.width Element.fill
        , Element.spacing 8
        ]
        [ Element.el [ Element.Font.size 17, Element.Font.semiBold ] <| Element.text title
        , element
        ]


viewTradeRow : Time.Posix -> Contracts.Types.FullTradeInfo -> Element Msg
viewTradeRow time trade =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 1
        ]
        (List.map cellMaker
            [ ( 2, viewExpiring time trade )
            , ( 2, viewTradeAmount trade )
            , ( 2, viewFiat trade )
            , ( 1, viewMargin trade )
            , ( 6, viewPaymentMethods trade )
            , ( 2, viewAutoabortWindow trade )
            , ( 2, viewAutoreleaseWindow trade )
            , ( 2, viewTradeButton trade.factoryID )
            ]
        )


cellMaker : ( Int, Element Msg ) -> Element Msg
cellMaker ( portion, cellElement ) =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height <| Element.px 70
        , Element.clip
        , Element.Background.color EH.white
        ]
    <|
        Element.el
            [ Element.padding 12
            , Element.centerY
            , Element.width Element.fill
            ]
            cellElement


viewExpiring : Time.Posix -> Contracts.Types.FullTradeInfo -> Element Msg
viewExpiring time trade =
    let
        interval =
            TimeHelpers.sub
                (TimeHelpers.add trade.state.phaseStartTime trade.parameters.autorecallInterval)
                time
    in
    EH.intervalWithElapsedBar interval trade.parameters.autorecallInterval Element.fill


viewTradeAmount : Contracts.Types.FullTradeInfo -> Element Msg
viewTradeAmount trade =
    EH.tokenValue trade.parameters.tradeAmount


viewFiat : Contracts.Types.FullTradeInfo -> Element Msg
viewFiat trade =
    EH.fiatValue trade.parameters.fiatPrice


viewMargin : Contracts.Types.FullTradeInfo -> Element Msg
viewMargin trade =
    trade.derived.margin
        |> Maybe.map
            (Utils.marginToString >> Element.text)
        |> Maybe.withDefault Element.none


viewPaymentMethods : Contracts.Types.FullTradeInfo -> Element Msg
viewPaymentMethods trade =
    Element.row [ Element.padding 3 ]
        (trade.parameters.paymentMethods
            |> List.map PaymentMethods.demoView
        )


viewAutoabortWindow : Contracts.Types.FullTradeInfo -> Element Msg
viewAutoabortWindow trade =
    EH.interval trade.parameters.autoabortInterval


viewAutoreleaseWindow : Contracts.Types.FullTradeInfo -> Element Msg
viewAutoreleaseWindow trade =
    EH.interval trade.parameters.autoreleaseInterval


viewTradeButton : Int -> Element Msg
viewTradeButton factoryID =
    Element.Input.button
        [ Element.Background.color <| Element.rgba255 16 7 234 0.2
        , Element.padding 11
        , Element.Border.rounded 4
        , Element.width Element.fill
        ]
        { onPress = Just <| TradeClicked factoryID
        , label =
            Element.el [ Element.centerX, Element.Font.color <| Element.rgb255 16 7 234 ] <| Element.text "View Offer"
        }


getLoadedTrades : List Contracts.Types.Trade -> List Contracts.Types.FullTradeInfo
getLoadedTrades =
    List.filterMap
        (\trade ->
            case trade of
                Contracts.Types.Loaded tradeInfo ->
                    Just tradeInfo

                _ ->
                    Nothing
        )


filterAndSortTrades :
    Time.Posix
    -> (Time.Posix -> Contracts.Types.FullTradeInfo -> Bool)
    -> (Contracts.Types.FullTradeInfo -> Contracts.Types.FullTradeInfo -> Order)
    -> List Contracts.Types.FullTradeInfo
    -> List Contracts.Types.FullTradeInfo
filterAndSortTrades time filterFunc sortFunc =
    List.filter (filterFunc time)
        >> List.sortWith sortFunc


sortableColumnHeader : String -> ResultColumnType -> Maybe Bool -> Element Msg
sortableColumnHeader title colType sorting =
    Element.row [ Element.spacing 8 ]
        [ columnHeader title
        , Element.column
            [ Element.spacing 2 ]
            [ Element.el
                [ Element.padding 4
                , Element.pointer
                , Element.Events.onClick <|
                    SortBy colType True
                ]
                (Element.image
                    [ Element.width <| Element.px 8
                    , Element.centerX
                    , Element.centerY
                    ]
                    { src = "static/img/sort-up-active.svg"
                    , description = "sort up"
                    }
                )
            , Element.el
                [ Element.padding 4
                , Element.pointer
                , Element.Events.onClick <|
                    SortBy colType False
                ]
                (Element.image
                    [ Element.width <| Element.px 8
                    , Element.centerX
                    , Element.centerY
                    ]
                    { src = "static/img/sort-down-active.svg"
                    , description = "sort down"
                    }
                )
            ]
        ]


columnHeader : String -> Element Msg
columnHeader title =
    Element.el [ Element.Font.semiBold, Element.Font.size 17 ] <| Element.text title
