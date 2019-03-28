module Trade.View exposing (root)

import Array
import CommonTypes exposing (UserInfo)
import Contracts.Types as CTypes exposing (FullTradeInfo)
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElementHelpers as EH
import Eth.Utils
import FiatValue exposing (FiatValue)
import Images exposing (Image)
import Margin
import PaymentMethods exposing (PaymentMethod)
import Time
import TokenValue exposing (TokenValue)
import Trade.Types exposing (..)


root : Time.Posix -> Model -> Element.Element Msg
root time model =
    case model.trade of
        CTypes.LoadedTrade tradeInfo ->
            Element.column
                [ Element.width Element.fill
                , Element.spacing 40
                ]
                [ header tradeInfo model.stats model.userInfo
                , Element.column
                    [ Element.width Element.fill
                    , Element.paddingXY 40 0
                    , Element.spacing 40
                    ]
                    [ phasesElement tradeInfo model.userInfo
                    , case tradeInfo.paymentMethods of
                        Err s ->
                            Element.paragraph
                                [ Element.Font.color EH.red
                                , Element.Font.size 24
                                ]
                                [ Element.text "Error decoding paymentMethods. This Trade was probably created on a third-party tool." ]

                        Ok paymentMethods ->
                            PaymentMethods.viewList paymentMethods Nothing
                    ]
                ]

        CTypes.PartiallyLoadedTrade partialTradeInfo ->
            Element.el
                [ Element.centerX
                , Element.centerY
                , Element.Font.size 30
                ]
                (Element.text "Loading contract info...")


header : FullTradeInfo -> StatsModel -> Maybe UserInfo -> Element Msg
header trade stats maybeUserInfo =
    EH.niceFloatingRow
        [ tradeStatusElement trade
        , daiAmountElement trade maybeUserInfo
        , fiatElement trade
        , marginElement trade maybeUserInfo
        , statsElement stats
        , case maybeUserInfo of
            Just userInfo ->
                actionButtonsElement trade userInfo

            Nothing ->
                Element.none
        ]


tradeStatusElement : FullTradeInfo -> Element Msg
tradeStatusElement trade =
    EH.withHeader
        "Trade Status"
        (Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text
                (case trade.state.phase of
                    CTypes.Created ->
                        "Created (uninitialized)"

                    CTypes.Open ->
                        case trade.parameters.openMode of
                            CTypes.BuyerOpened ->
                                "Open Buy Offer"

                            CTypes.SellerOpened ->
                                "Open Sell Offer"

                    CTypes.Committed ->
                        "Committed"

                    CTypes.Claimed ->
                        "Claimed"

                    CTypes.Closed ->
                        "Closed"
                )
            )
        )


daiAmountElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
daiAmountElement trade maybeUserInfo =
    let
        maybeInitiatorOrResponder =
            Maybe.andThen
                (getInitiatorOrResponder trade)
                (Maybe.map .address maybeUserInfo)
    in
    EH.withHeader
        (case ( trade.parameters.openMode, maybeInitiatorOrResponder ) of
            ( CTypes.BuyerOpened, Just Initiator ) ->
                "You're Buying"

            ( CTypes.BuyerOpened, _ ) ->
                "Buying"

            ( CTypes.SellerOpened, Just Initiator ) ->
                "You're Selling"

            ( CTypes.SellerOpened, _ ) ->
                "Selling"
        )
        (renderDaiAmount trade.parameters.tradeAmount)


renderDaiAmount : TokenValue -> Element Msg
renderDaiAmount daiAmount =
    Element.row
        [ Element.spacing 8 ]
        [ Images.toElement [] Images.daiSymbol
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text <| TokenValue.toConciseString daiAmount)
        ]


fiatElement : FullTradeInfo -> Element Msg
fiatElement trade =
    EH.withHeader
        "For Fiat"
        (renderFiatAmount trade.parameters.fiatPrice)


renderFiatAmount : FiatValue -> Element Msg
renderFiatAmount fiatValue =
    Element.row
        [ Element.spacing 5 ]
        [ FiatValue.typeStringToSymbol fiatValue.fiatType
        , Element.el
            [ Element.Font.size 24
            , Element.Font.medium
            ]
            (Element.text <| FiatValue.renderToString fiatValue)
        ]


marginElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
marginElement trade maybeUserInfo =
    EH.withHeader
        "At Margin"
        (case trade.derived.margin of
            Just marginFloat ->
                renderMargin marginFloat maybeUserInfo

            Nothing ->
                EH.comingSoonMsg [] "Margin for non-USD currencies coming soon!"
        )


renderMargin : Float -> Maybe UserInfo -> Element Msg
renderMargin marginFloat maybeUserInfo =
    let
        marginString =
            Margin.marginToString marginFloat ++ "%"

        image =
            if marginFloat == 0 then
                Images.none

            else
                Images.marginSymbol (marginFloat > 0) Nothing
    in
    Element.row [ Element.spacing 5 ]
        [ Element.text marginString
        , Images.toElement [] image
        ]


statsElement : StatsModel -> Element Msg
statsElement stats =
    EH.withHeader
        "Initiator Stats"
        (EH.comingSoonMsg [] "Stats coming soon!")


actionButtonsElement : FullTradeInfo -> UserInfo -> Element Msg
actionButtonsElement trade userInfo =
    Element.row
        [ Element.spacing 8 ]
        (case
            ( trade.state.phase
            , getInitiatorOrResponder trade userInfo.address
            , getBuyerOrSeller trade userInfo.address
            )
         of
            ( CTypes.Created, _, _ ) ->
                []

            ( CTypes.Open, Just Initiator, _ ) ->
                [ EH.blueButton "Remove and Refund this Trade" Recall ]

            ( CTypes.Open, Nothing, _ ) ->
                [ EH.redButton "Deposit and Commit to Trade" Commit ]

            ( CTypes.Committed, _, Just Buyer ) ->
                [ EH.orangeButton "Abort Trade" Abort
                , EH.redButton "I Confirm I have Sent Payment" Claim
                ]

            ( CTypes.Claimed, _, Just Seller ) ->
                [ EH.redButton "Burn it all" Burn
                , EH.blueButton "Release Everything Now" Release
                ]

            _ ->
                []
        )
        |> Element.map ContractAction


phasesElement : FullTradeInfo -> Maybe UserInfo -> Element Msg
phasesElement trade maybeUserInfo =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 300
        , Element.Background.color EH.white
        , Element.Border.rounded 8
        ]
        (Element.el
            [ Element.centerX
            , Element.centerY
            ]
            (EH.comingSoonMsg [ Element.Font.size 30 ] "Phases info readout coming soon!")
        )
