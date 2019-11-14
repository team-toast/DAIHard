module AgentHistory.View exposing (root)

import AgentHistory.Types exposing (..)
import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Currencies exposing (Price)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import Eth.Utils
import Filters.Types as Filters
import Filters.View as Filters
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Html.Events.Extra
import Images exposing (Image)
import Maybe.Extra
import PaymentMethods exposing (PaymentMethod)
import Time
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types as TradeCache exposing (TradeCache)
import TradeTable.Types as TradeTable
import TradeTable.View as TradeTable
import Wallet


root : Time.Posix -> DisplayProfile -> List TradeCache -> Model -> Element Msg
root time dProfile tradeCaches model =
    EH.simpleSubmodelContainer
        1800
        (Element.column
            [ Element.width Element.fill
            , Element.padding (30 |> changeForMobile 10 dProfile)
            ]
            [ titleElement dProfile model
            , statusAndFiltersElement dProfile tradeCaches model
            , let
                tcDoneLoading =
                    List.all
                        (TradeCache.loadingStatus >> (==) TradeCache.AllFetched)
                        tradeCaches
              in
              maybeResultsElement time dProfile tcDoneLoading tradeCaches model
            ]
        )


titleElement : DisplayProfile -> Model -> Element Msg
titleElement dProfile model =
    let
        viewingOwnHistory =
            case Wallet.userInfo model.wallet of
                Nothing ->
                    False

                Just userInfo ->
                    userInfo.address == model.agentAddress
    in
    if viewingOwnHistory then
        Element.none

    else
        (Element.row |> changeForMobile Element.column dProfile)
            [ Element.spacing 10
            , Element.centerX
            , Element.paddingEach
                { top = 10
                , left = 20
                , right = 20
                , bottom = 20
                }
            ]
            [ Element.el
                [ Element.Font.size (24 |> changeForMobile 18 dProfile)
                , Element.centerX
                , Element.Font.semiBold
                ]
                (Element.text "Trade History for User")
            , EH.ethAddress (18 |> changeForMobile 12 dProfile) model.agentAddress
            ]


statusAndFiltersElement : DisplayProfile -> List TradeCache -> Model -> Element Msg
statusAndFiltersElement dProfile tradeCaches model =
    let
        statusMsgElement s =
            Element.el
                [ Element.Font.size 20
                , Element.Font.semiBold
                , Element.Font.color EH.darkGray
                , Element.centerX
                ]
                (Element.text s)

        statusMessages : List (Element Msg)
        statusMessages =
            if List.all ((==) TradeCache.NoneFound) (List.map TradeCache.loadingStatus tradeCaches) then
                [ statusMsgElement "No trades found." ]

            else
                tradeCaches
                    |> List.map
                        (\tc ->
                            case TradeCache.loadingStatus tc of
                                TradeCache.QueryingNumTrades ->
                                    Just <| "Querying " ++ factoryName tc.factory ++ " Factory..."

                                TradeCache.NoneFound ->
                                    Nothing

                                TradeCache.FetchingTrades ->
                                    Just <| "Fetching " ++ factoryName tc.factory ++ " Trades..."

                                TradeCache.AllFetched ->
                                    Nothing
                        )
                    |> Maybe.Extra.values
                    |> List.map statusMsgElement
    in
    Element.column
        [ Element.width Element.fill
        , Element.padding 10
        , Element.spacing 10
        ]
        [ Element.el
            [ Element.centerX ]
            (Element.map FiltersMsg <| Filters.view dProfile model.filters)
        , Element.column
            [ Element.spacing 5
            , Element.alignLeft
            , Element.centerX
            ]
            statusMessages
        ]


maybeResultsElement : Time.Posix -> DisplayProfile -> Bool -> List TradeCache -> Model -> Element Msg
maybeResultsElement time dProfile tcDoneLoading tradeCaches model =
    let
        visibleTrades =
            tradeCaches
                |> List.map
                    (\tradeCache ->
                        TradeCache.loadedValidTrades tradeCache
                            |> filterTrades
                                (basicFilterFunc model)
                    )
                |> List.concat
                |> Filters.filterTrades model.filters
    in
    if visibleTrades == [] then
        Element.el
            [ Element.centerX
            , Element.Font.size (24 |> changeForMobile 16 dProfile)
            , Element.paddingEach
                { top = 30
                , left = 0
                , right = 0
                , bottom = 0
                }
            , Element.Font.italic
            ]
            (if tcDoneLoading then
                Element.text "No trades found with those filters."

             else
                Element.text "Initializing Trade Cache..."
            )

    else
        let
            cols =
                case dProfile of
                    Desktop ->
                        [ TradeTable.Phase
                        , TradeTable.Offer
                        , TradeTable.ResponderProfit
                        , TradeTable.PaymentWindow
                        , TradeTable.BurnWindow
                        ]

                    Mobile ->
                        [ TradeTable.Offer
                        , TradeTable.Windows
                        ]
        in
        TradeTable.view
            time
            dProfile
            model.tradeTable
            model.prices
            cols
            visibleTrades
            |> Element.map TradeTableMsg


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


filterTrades :
    (CTypes.FullTradeInfo -> Bool)
    -> List CTypes.FullTradeInfo
    -> List CTypes.FullTradeInfo
filterTrades filterFunc =
    List.filter filterFunc


basicFilterFunc : Model -> (CTypes.FullTradeInfo -> Bool)
basicFilterFunc model =
    \trade ->
        (trade.parameters.initiatorAddress == model.agentAddress)
            || (trade.state.responder == Just model.agentAddress)
