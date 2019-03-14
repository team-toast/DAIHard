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
import Search.Types exposing (..)
import Time
import TimeHelpers


root : Time.Posix -> Model -> Element Msg
root time model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ searchInputElement model.inputs
        , EH.hbreak
        , resultsElement time model
        ]


searchInputElement : SearchInputs -> Element Msg
searchInputElement inputs =
    Element.row
        [ Element.width Element.fill
        , Element.height <| Element.px 100
        , Element.spacing 10
        , Element.padding 30
        ]
        [ Element.el [ Element.width <| Element.fillPortion 3 ] <|
            daiRangeInput inputs.daiRange
        , Element.el [ Element.width <| Element.fillPortion 2 ] <|
            fiatTypeInput inputs.fiatType
        , Element.el [ Element.width <| Element.fillPortion 3 ] <|
            fiatRangeInput inputs.fiatRange
        , Element.el [ Element.width <| Element.fillPortion 6 ] <|
            paymentMethodsInput inputs.paymentMethod
        , Element.el [ Element.width <| Element.fillPortion 6 ] <|
            locationInput inputs.location
        , Element.el [] <|
            resetButton
        ]


resultsElement : Time.Posix -> Model -> Element Msg
resultsElement time model =
    let
        visibleTrades =
            model.trades
                |> Array.toList
                |> getLoadedTrades
                |> filterAndSortTrades time model.filterFunc model.sortFunc
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        , Element.spacing 20
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ cellMaker ( 2, sortableColumnHeader Expiring "Offer Expires" Nothing )
            , cellMaker ( 2, sortableColumnHeader TradeAmount "Trading" Nothing )
            , cellMaker ( 2, sortableColumnHeader Fiat "For Fiat" Nothing )
            , cellMaker ( 1, sortableColumnHeader Margin "Margin" Nothing )
            , cellMaker ( 6, sortableColumnHeader PaymentMethods "Accepted Payment Methods" Nothing )
            , cellMaker ( 2, sortableColumnHeader AutoabortWindow "Payment Window" Nothing )
            , cellMaker ( 2, sortableColumnHeader AutoreleaseWindow "Auto-Release" Nothing )
            , cellMaker ( 1, Element.none )
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
    dummyTextInput
        |> withInputHeader "Dai Amount"


fiatTypeInput : Maybe FiatType -> Element Msg
fiatTypeInput fiatType =
    dummyTextInput
        |> withInputHeader "Fiat Type"


fiatRangeInput : AmountRange -> Element Msg
fiatRangeInput range =
    dummyTextInput
        |> withInputHeader "Fiat Amount"


paymentMethodsInput : Maybe PaymentMethodQuery -> Element Msg
paymentMethodsInput pmQuery =
    dummyTextInput
        |> withInputHeader "Payment Methods"


locationInput : Maybe LocationQuery -> Element Msg
locationInput lQuery =
    dummyTextInput
        |> withInputHeader "Location"


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
        { onPress = Just NoOp
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
            , ( 1, viewTradeButton trade.factoryID )
            ]
        )


cellMaker : ( Int, Element Msg ) -> Element Msg
cellMaker ( portion, cellElement ) =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.clip
        , Element.Background.color EH.white
        ]
    <|
        Element.el
            [ Element.padding 12 ]
            cellElement


viewExpiring : Time.Posix -> Contracts.Types.FullTradeInfo -> Element Msg
viewExpiring time trade =
    Element.text
        (EH.secondsRemainingString
            (TimeHelpers.add trade.state.phaseStartTime trade.parameters.autorecallInterval)
            time
        )


viewTradeAmount : Contracts.Types.FullTradeInfo -> Element Msg
viewTradeAmount trade =
    EH.tokenValue trade.parameters.tradeAmount


viewFiat : Contracts.Types.FullTradeInfo -> Element Msg
viewFiat trade =
    EH.fiat trade.parameters.fiatType trade.parameters.fiatPrice


viewMargin : Contracts.Types.FullTradeInfo -> Element Msg
viewMargin trade =
    Element.text "??"


viewPaymentMethods : Contracts.Types.FullTradeInfo -> Element Msg
viewPaymentMethods trade =
    Element.text "??"


viewAutoabortWindow : Contracts.Types.FullTradeInfo -> Element Msg
viewAutoabortWindow trade =
    Element.text "??"


viewAutoreleaseWindow : Contracts.Types.FullTradeInfo -> Element Msg
viewAutoreleaseWindow trade =
    Element.text "??"


viewTradeButton : Int -> Element Msg
viewTradeButton factoryID =
    Element.text "??"


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


sortableColumnHeader : ResultColumnType -> String -> Maybe Bool -> Element Msg
sortableColumnHeader colType title sorting =
    Element.row [ Element.spacing 8 ]
        [ Element.el [ Element.Font.semiBold, Element.Font.size 17 ] <| Element.text title
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
