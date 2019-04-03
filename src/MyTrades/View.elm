module MyTrades.View exposing (root)

import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import ElementHelpers as EH
import FiatValue exposing (FiatValue)
import Html.Events.Extra
import Images exposing (Image)
import Margin
import MyTrades.Types exposing (..)
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TradeCache.State as TradeCache


root : Time.Posix -> Model -> Element Msg
root time model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        ]
        [ EH.comingSoonMsg [ Element.Font.size 20 ] "Sorted and filtered views coming soon!"
        , resultsElement time model
        ]


basicFilterFunc : Model -> (Time.Posix -> CTypes.FullTradeInfo -> Bool)
basicFilterFunc model =
    \time trade ->
        case model.userInfo of
            Just userInfo ->
                (trade.parameters.initiatorAddress == userInfo.address)
                    || (trade.state.responder == Just userInfo.address)

            Nothing ->
                False


basicSortFunc : Model -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
basicSortFunc model =
    \a b ->
        let
            phaseOrder =
                compare
                    (CTypes.phaseToInt a.state.phase)
                    (CTypes.phaseToInt b.state.phase)
        in
        if phaseOrder == EQ then
            phaseOrder

        else
            TimeHelpers.compare
                a.derived.phaseEndTime
                b.derived.phaseEndTime


resultsElement : Time.Posix -> Model -> Element Msg
resultsElement time model =
    let
        visibleTrades =
            TradeCache.loadedTrades model.tradeCache
                |> filterAndSortTrades
                    time
                    (basicFilterFunc model)
                    (basicSortFunc model)
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.padding 30
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ cellMaker ( 1, columnHeader "Expires" )
            , cellMaker ( 1, columnHeader "Trading" )
            , cellMaker ( 2, columnHeader "For Fiat" )
            , cellMaker ( 1, columnHeader "Margin" )
            , cellMaker ( 6, columnHeader "Accepted Payment Methods" )
            , cellMaker ( 2, columnHeader "Payment Window" )
            , cellMaker ( 2, columnHeader "Auto-Release" )
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


viewTradeRow : Time.Posix -> CTypes.FullTradeInfo -> Element Msg
viewTradeRow time trade =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 1
        ]
        (List.map cellMaker
            [ ( 1, viewExpiring time trade )
            , ( 1, viewTradeAmount trade )
            , ( 2, viewFiat trade )
            , ( 1, viewMargin trade )
            , ( 6, viewPaymentMethods trade.paymentMethods )
            , ( 2, viewAutoabortWindow trade )
            , ( 2, viewAutoreleaseWindow trade )
            , ( 2, viewTradeButton trade.factoryID )
            ]
        )


cellMaker : ( Int, Element Msg ) -> Element Msg
cellMaker ( portion, cellElement ) =
    Element.el
        [ Element.width <| Element.fillPortion portion
        , Element.height <| Element.px 60
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


viewExpiring : Time.Posix -> CTypes.FullTradeInfo -> Element Msg
viewExpiring time trade =
    let
        interval =
            TimeHelpers.sub
                (TimeHelpers.add trade.state.phaseStartTime trade.parameters.autorecallInterval)
                time
    in
    EH.smallIntervalWithElapsedBar interval trade.parameters.autorecallInterval Element.fill


viewTradeAmount : CTypes.FullTradeInfo -> Element Msg
viewTradeAmount trade =
    EH.daiValue trade.parameters.tradeAmount


viewFiat : CTypes.FullTradeInfo -> Element Msg
viewFiat trade =
    EH.fiatValue trade.parameters.fiatPrice


viewMargin : CTypes.FullTradeInfo -> Element Msg
viewMargin trade =
    EH.comingSoonMsg [] "):<"



-- trade.derived.margin
--     |> Maybe.map (EH.margin upIsGreen)
--     |> Maybe.withDefault Element.none


viewPaymentMethods : List PaymentMethod -> Element Msg
viewPaymentMethods paymentMethods =
    EH.comingSoonMsg [] "Payment method summary coming soon! For now, click \"View offer\" ---> "


viewAutoabortWindow : CTypes.FullTradeInfo -> Element Msg
viewAutoabortWindow trade =
    EH.interval False (Just EH.red) trade.parameters.autoabortInterval


viewAutoreleaseWindow : CTypes.FullTradeInfo -> Element Msg
viewAutoreleaseWindow trade =
    EH.interval False (Just EH.red) trade.parameters.autoreleaseInterval


viewTradeButton : Int -> Element Msg
viewTradeButton factoryID =
    Element.Input.button
        [ Element.Background.color <| Element.rgba255 16 7 234 0.2
        , Element.padding 11
        , Element.Border.rounded 4
        , Element.width Element.fill
        , Element.mouseOver [ Element.Background.color <| Element.rgba255 16 7 234 0.4 ]
        ]
        { onPress = Just <| TradeClicked factoryID
        , label =
            Element.el [ Element.centerX, Element.Font.color <| Element.rgb255 16 7 234, Element.Font.medium ] <| Element.text "View Offer"
        }


getLoadedTrades : List CTypes.Trade -> List CTypes.FullTradeInfo
getLoadedTrades =
    List.filterMap
        (\trade ->
            case trade of
                CTypes.LoadedTrade tradeInfo ->
                    Just tradeInfo

                _ ->
                    Nothing
        )


filterAndSortTrades :
    Time.Posix
    -> (Time.Posix -> CTypes.FullTradeInfo -> Bool)
    -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
    -> List CTypes.FullTradeInfo
    -> List CTypes.FullTradeInfo
filterAndSortTrades time filterFunc sortFunc =
    List.filter (filterFunc time)
        >> List.sortWith sortFunc


columnHeader : String -> Element Msg
columnHeader title =
    Element.el [ Element.Font.medium, Element.Font.size 17 ] <| Element.text title
