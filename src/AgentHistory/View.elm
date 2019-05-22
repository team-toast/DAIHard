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
import ElementHelpers as EH
import Eth.Types exposing (Address)
import FiatValue exposing (FiatValue)
import Html.Events.Extra
import Images exposing (Image)
import Margin
import PaymentMethods exposing (PaymentMethod)
import Time
import TimeHelpers
import TokenValue exposing (TokenValue)
import TradeCache.State as TradeCache
import TradeCache.Types exposing (TradeCache)


root : Time.Posix -> TradeCache -> Model -> Element Msg
root time tradeCache model =
    Element.column
        [ Element.Border.rounded 5
        , Element.Background.color EH.white
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 0 20
        ]
        [ pageTitleElement model
        , viewTypeElement model
        , phaseElement model
        , resultsElement time tradeCache model
        ]


pageTitleElement : Model -> Element Msg
pageTitleElement model =
    let
        viewingOwnHistory =
            case model.userInfo of
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


viewTypeElement : Model -> Element Msg
viewTypeElement model =
    Element.el
        [ Element.paddingXY 30 10 ]
        (userRoleToggle model.agentRole)


userRoleToggle : BuyerOrSeller -> Element Msg
userRoleToggle buyerOrSeller =
    let
        baseStyles =
            [ Element.Font.size 24
            , Element.Font.semiBold
            , Element.pointer
            ]

        ( asBuyerStyles, asSellerStyles ) =
            case buyerOrSeller of
                Buyer ->
                    ( baseStyles
                    , baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    )

                Seller ->
                    ( baseStyles ++ [ Element.Font.color EH.disabledTextColor ]
                    , baseStyles
                    )
    in
    Element.row [ Element.spacing 20 ]
        [ Element.el
            ([ Element.Events.onClick <| ViewUserRoleChanged Seller ] ++ asSellerStyles)
            (Element.text "As the Seller")
        , Element.el
            ([ Element.Events.onClick <| ViewUserRoleChanged Buyer ] ++ asBuyerStyles)
            (Element.text "As the Buyer")
        ]


phaseElement : Model -> Element Msg
phaseElement model =
    Element.el
        [ Element.paddingXY 30 10 ]
        (choosePhaseElement model.viewPhase)


choosePhaseElement : CTypes.Phase -> Element Msg
choosePhaseElement activePhase =
    let
        baseStyles =
            [ Element.Font.size 20
            , Element.Font.bold
            , Element.pointer
            ]

        phaseButtonStyles isActive =
            if isActive then
                baseStyles ++ [ Element.Font.color EH.blue ]

            else
                baseStyles
    in
    Element.row [ Element.spacing 30 ]
        [ Element.el
            ([ Element.Events.onClick <| ViewPhaseChanged CTypes.Open ]
                ++ phaseButtonStyles (activePhase == CTypes.Open)
            )
            (Element.text "Open")
        , Element.el
            ([ Element.Events.onClick <| ViewPhaseChanged CTypes.Committed ]
                ++ phaseButtonStyles (activePhase == CTypes.Committed)
            )
            (Element.text "Payment Due")
        , Element.el
            ([ Element.Events.onClick <| ViewPhaseChanged CTypes.Claimed ]
                ++ phaseButtonStyles (activePhase == CTypes.Claimed)
            )
            (Element.text "Release Due")
        , Element.el
            ([ Element.Events.onClick <| ViewPhaseChanged CTypes.Closed ]
                ++ phaseButtonStyles (activePhase == CTypes.Closed)
            )
            (Element.text "Closed")
        ]


tradeMatchesUserRole : CTypes.FullTradeInfo -> BuyerOrSeller -> Address -> Bool
tradeMatchesUserRole trade role userAddress =
    CTypes.getBuyerOrSeller trade userAddress == Just role


resultsElement : Time.Posix -> TradeCache -> Model -> Element Msg
resultsElement time tradeCache model =
    let
        userTrades =
            TradeCache.loadedTrades tradeCache
                |> filterAndSortTrades
                    (basicFilterFunc model)
                    (basicSortFunc model)

        visibleTrades =
            userTrades
                |> List.filter
                    (\trade ->
                        tradeMatchesUserRole trade model.agentRole model.agentAddress
                            && (trade.state.phase == model.viewPhase)
                    )

        amountTitleString =
            case model.agentRole of
                Buyer ->
                    "Buying"

                Seller ->
                    "Selling"

        phaseCountdownTitleString =
            case model.viewPhase of
                CTypes.Open ->
                    "Expires in"

                CTypes.Committed ->
                    "Payment Due"

                CTypes.Claimed ->
                    "Auto-Release"

                CTypes.Closed ->
                    ""
    in
    Element.column
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.paddingXY 30 10
        , Element.spacing 5
        ]
        [ Element.row
            [ Element.width Element.fill ]
            [ if model.viewPhase /= CTypes.Closed then
                cellMaker ( 1, columnHeader phaseCountdownTitleString )

              else
                Element.none
            , cellMaker ( 1, columnHeader amountTitleString )
            , cellMaker ( 2, columnHeader "For Fiat" )
            , cellMaker ( 1, columnHeader "Margin" )
            , cellMaker ( 6, columnHeader "Payment Methods" )
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
                    (viewTradeRow time model.agentRole model.viewPhase)
            )
        ]


viewTradeRow : Time.Posix -> BuyerOrSeller -> CTypes.Phase -> CTypes.FullTradeInfo -> Element Msg
viewTradeRow time userRole viewPhase trade =
    Element.row
        [ Element.width Element.fill
        , Element.spacing 1
        ]
        [ case viewPhase of
            CTypes.Open ->
                cellMaker ( 1, phaseCountdown time trade False )

            CTypes.Committed ->
                cellMaker ( 1, phaseCountdown time trade (userRole == Buyer) )

            CTypes.Claimed ->
                cellMaker ( 1, phaseCountdown time trade (userRole == Seller) )

            CTypes.Closed ->
                Element.none
        , cellMaker ( 1, viewTradeAmount trade.parameters.tradeAmount )
        , cellMaker ( 2, viewFiat trade.terms.price )
        , cellMaker ( 1, viewMargin trade (userRole == Seller) )
        , cellMaker ( 6, viewPaymentMethods trade.terms.paymentMethods )
        , cellMaker ( 2, viewTradeButton trade.factoryID )
        ]


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


phaseCountdown : Time.Posix -> CTypes.FullTradeInfo -> Bool -> Element Msg
phaseCountdown time trade userActionNeeded =
    case CTypes.getCurrentPhaseTimeoutInfo time trade of
        CTypes.TimeLeft timeoutInfo ->
            let
                baseColor =
                    if userActionNeeded && (TimeHelpers.getRatio (Tuple.first timeoutInfo) (Tuple.second timeoutInfo) < 0.05) then
                        EH.red

                    else
                        EH.black
            in
            EH.intervalWithElapsedBar
                [ Element.width Element.fill ]
                [ Element.Font.size 16 ]
                ( baseColor, EH.lightGray )
                timeoutInfo

        CTypes.TimeUp _ ->
            Element.column
                [ Element.spacing 4
                , Element.width Element.fill
                ]
                [ Element.el
                    [ Element.centerX
                    , Element.Font.size 14
                    ]
                    (Element.text (CTypes.getPokeText trade.state.phase))
                , Element.el
                    [ Element.centerX ]
                    (pokeButton trade.creationInfo.address)
                ]


pokeButton : Address -> Element Msg
pokeButton address =
    Element.Input.button
        [ Element.Background.color <| Element.rgba255 16 7 234 0.2
        , Element.padding 5
        , Element.Border.rounded 4
        , Element.width Element.fill
        , Element.mouseOver [ Element.Background.color <| Element.rgba255 16 7 234 0.4 ]
        ]
        { onPress = Just <| Poke address
        , label =
            Element.el
                [ Element.centerX
                , Element.Font.color <| Element.rgb255 16 7 234
                , Element.Font.medium
                , Element.Font.size 14
                ]
                (Element.text "Poke")
        }


viewTradeAmount : TokenValue -> Element Msg
viewTradeAmount tradeAmount =
    EH.daiValue tradeAmount


viewFiat : FiatValue -> Element Msg
viewFiat price =
    EH.fiatValue price


viewMargin : CTypes.FullTradeInfo -> Bool -> Element Msg
viewMargin trade upIsGreen =
    trade.derived.margin
        |> Maybe.map (EH.coloredMargin upIsGreen)
        |> Maybe.withDefault Element.none


viewPaymentMethods : List PaymentMethod -> Element Msg
viewPaymentMethods paymentMethods =
    paymentMethods
        |> List.head
        |> Maybe.map PaymentMethods.previewTextHack
        |> Maybe.withDefault Element.none


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
    (CTypes.FullTradeInfo -> Bool)
    -> (CTypes.FullTradeInfo -> CTypes.FullTradeInfo -> Order)
    -> List CTypes.FullTradeInfo
    -> List CTypes.FullTradeInfo
filterAndSortTrades filterFunc sortFunc =
    List.filter filterFunc
        >> List.sortWith sortFunc


columnHeader : String -> Element Msg
columnHeader title =
    Element.el [ Element.Font.medium, Element.Font.size 17 ] <| Element.text title


basicFilterFunc : Model -> (CTypes.FullTradeInfo -> Bool)
basicFilterFunc model =
    \trade ->
        (trade.parameters.initiatorAddress == model.agentAddress)
            || (trade.state.responder == Just model.agentAddress)


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
