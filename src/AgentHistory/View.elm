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


root : Time.Posix -> List TradeCache -> Model -> Element Msg
root time tradeCaches model =
    EH.simpleSubmodelContainer
        1800
        (Element.column
            [ Element.width Element.fill
            , Element.padding 30
            ]
            [ titleElement model
            , statusAndFiltersElement tradeCaches model
            , let
                tcDoneLoading =
                    List.all
                        (TradeCache.loadingStatus >> (==) TradeCache.AllFetched)
                        tradeCaches
              in
              maybeResultsElement time tcDoneLoading tradeCaches model
            ]
        )


titleElement : Model -> Element Msg
titleElement model =
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
        Element.row
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
                [ Element.Font.size 24
                , Element.Font.semiBold
                ]
                (Element.text "Trade History for User")
            , EH.ethAddress 18 model.agentAddress
            ]


statusAndFiltersElement : List TradeCache -> Model -> Element Msg
statusAndFiltersElement tradeCaches model =
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
    Element.el
        [ Element.width Element.fill
        , Element.inFront <|
            Element.column
                [ Element.spacing 5
                , Element.alignLeft
                ]
                statusMessages
        ]
        (Element.el
            [ Element.centerX ]
            (Element.map FiltersMsg <| Filters.view model.filters)
        )


maybeResultsElement : Time.Posix -> Bool -> List TradeCache -> Model -> Element Msg
maybeResultsElement time tcDoneLoading tradeCaches model =
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
            , Element.Font.size 24
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
        TradeTable.view
            time
            model.tradeTable
            model.prices
            [ TradeTable.Phase
            , TradeTable.Offer
            , TradeTable.Price
            , TradeTable.ResponderProfit
            , TradeTable.PaymentWindow
            , TradeTable.BurnWindow
            ]
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
