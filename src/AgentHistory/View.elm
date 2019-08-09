module AgentHistory.View exposing (root)

import AgentHistory.Types exposing (..)
import Array exposing (Array)
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Helpers.Element as EH
import Helpers.Time as TimeHelpers
import Html.Events.Extra
import Images exposing (Image)
import Margin
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
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 0 20
        ]
        [ pageTitleElement model
        , resultsAndStatusElement time tradeCaches model
        ]


pageTitleElement : Model -> Element Msg
pageTitleElement model =
    let
        viewingOwnHistory =
            case Wallet.userInfo model.wallet of
                Nothing ->
                    False

                Just userInfo ->
                    userInfo.address == model.agentAddress
    in
    if viewingOwnHistory then
        Element.el
            [ Element.paddingXY 30 10
            , Element.Font.size 24
            , Element.Font.semiBold
            ]
            (Element.text "Your Trades")

    else
        Element.row
            [ Element.spacing 10
            , Element.paddingEach
                { top = 10
                , left = 30
                , right = 30
                , bottom = 30
                }
            ]
            [ Element.el
                [ Element.Font.size 24
                , Element.Font.semiBold
                ]
                (Element.text "Trade History for User")
            , EH.ethAddress 18 model.agentAddress
            ]


tradeMatchesUserRole : CTypes.FullTradeInfo -> BuyerOrSeller -> Address -> Bool
tradeMatchesUserRole trade role userAddress =
    CTypes.getBuyerOrSeller trade userAddress == Just role


resultsAndStatusElement : Time.Posix -> List TradeCache -> Model -> Element Msg
resultsAndStatusElement time tradeCaches model =
    let
        statusMsgElement s =
            Element.el
                [ Element.Font.size 24
                , Element.Font.semiBold
                , Element.Font.color EH.darkGray
                , Element.centerX
                , Element.padding 20
                ]
                (Element.text s)

        userTrades =
            tradeCaches
                |> List.map
                    (\tradeCache ->
                        TradeCache.loadedValidTrades tradeCache
                            |> filterTrades
                                (basicFilterFunc model)
                    )
                |> List.concat

        visibleTrades =
            userTrades

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
                                    Just <| factoryName tc.factory ++ "Querying Factory..."

                                TradeCache.NoneFound ->
                                    Nothing

                                TradeCache.FetchingTrades ->
                                    Just <| factoryName tc.factory ++ "Fetching Trades"

                                TradeCache.AllFetched ->
                                    Nothing
                        )
                    |> Maybe.Extra.values
                    |> List.map statusMsgElement
    in
    Element.column
        [ Element.spacing 10 ]
        [ case statusMessages of
            [] ->
                Element.none

            _ ->
                Element.column [ Element.spacing 5 ] statusMessages
        , maybeResultsElement time visibleTrades model
        ]


maybeResultsElement : Time.Posix -> List CTypes.FullTradeInfo -> Model -> Element Msg
maybeResultsElement time visibleTrades model =
    if visibleTrades == [] then
        Element.none

    else
        TradeTable.view
            time
            model.tradeTable
            [ TradeTable.Phase
            , TradeTable.Offer
            , TradeTable.FiatPrice
            , TradeTable.Margin
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
