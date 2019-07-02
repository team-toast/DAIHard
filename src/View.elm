module View exposing (root)

import AgentHistory.View
import Browser
import CommonTypes exposing (..)
import Contracts.Types as CTypes
import Create.View
import Dict
import Element exposing (Attribute, Element)
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import FiatValue
import Helpers.Element as EH
import Landing.View
import Marketplace.Types
import Marketplace.View
import Routing
import Trade.View
import Types exposing (..)


root : Model -> Browser.Document Msg
root maybeValidModel =
    { title = "DAIHard"
    , body =
        [ let
            mainElementAttributes =
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.scrollbarY
                , Element.Font.family
                    [ Element.Font.typeface "Soleil"
                    , Element.Font.sansSerif
                    ]
                ]
          in
          Element.layout
            mainElementAttributes
            (pageElement maybeValidModel)
        ]
    }


pageElement : Model -> Element Msg
pageElement maybeValidModel =
    Element.column
        [ Element.behindContent <| headerBackground
        , Element.inFront <| headerContent maybeValidModel
        , Element.width Element.fill
        , Element.height Element.fill
        , Element.Background.color EH.pageBackgroundColor
        , Element.padding 30
        , Element.scrollbarY
        ]
        [ Element.el
            [ Element.height (Element.px 50) ]
            Element.none
        , subModelElement maybeValidModel
        ]


headerBackground : Element Msg
headerBackground =
    Element.el
        [ Element.width Element.fill
        , Element.height <| Element.px 150
        , Element.Background.color EH.headerBackgroundColor
        ]
        Element.none


headerContent : Model -> Element Msg
headerContent maybeValidModel =
    let
        ( maybeCurrentSubmodel, maybeUserInfo ) =
            case maybeValidModel of
                Running model ->
                    ( Just model.submodel, model.userInfo )

                Failed _ ->
                    ( Nothing, Nothing )
    in
    Element.row
        [ Element.width Element.fill
        , Element.spacing 30
        , Element.paddingXY 30 17
        ]
        [ headerLink
            "Buy Dai"
            (GotoRoute <| Routing.Marketplace Buyer)
            (Maybe.map
                (\submodel ->
                    case submodel of
                        MarketplaceModel marketplaceModel ->
                            if marketplaceModel.browsingRole == Buyer then
                                Active

                            else
                                Normal

                        _ ->
                            Normal
                )
                maybeCurrentSubmodel
                |> Maybe.withDefault Normal
            )
        , headerLink
            "Sell Dai"
            (GotoRoute <| Routing.Marketplace Seller)
            (Maybe.map
                (\submodel ->
                    case submodel of
                        MarketplaceModel marketplaceModel ->
                            if marketplaceModel.browsingRole == Seller then
                                Active

                            else
                                Normal

                        _ ->
                            Normal
                )
                maybeCurrentSubmodel
                |> Maybe.withDefault Normal
            )
        , headerLink
            "Create a New Offer"
            (GotoRoute Routing.Create)
            (Maybe.map
                (\submodel ->
                    case submodel of
                        CreateModel _ ->
                            Active

                        _ ->
                            Normal
                )
                maybeCurrentSubmodel
                |> Maybe.withDefault Normal
            )
        , case maybeUserInfo of
            Just userInfo ->
                headerLink
                    "My Trades"
                    (GotoRoute <| Routing.AgentHistory userInfo.address Seller)
                    (Maybe.map
                        (\submodel ->
                            case submodel of
                                AgentHistoryModel agentHistoryModel ->
                                    if agentHistoryModel.agentAddress == userInfo.address then
                                        Active

                                    else
                                        Normal

                                _ ->
                                    Normal
                        )
                        maybeCurrentSubmodel
                        |> Maybe.withDefault Normal
                    )

            Nothing ->
                headerLink
                    "Connect to Wallet"
                    ConnectToWeb3
                    Important
        , Element.column
            [ Element.alignRight
            , Element.spacing 0
            , Element.paddingXY 8 0
            ]
            [ logoElement
            , networkModeElement maybeValidModel
            ]
        ]


networkModeElement : Model -> Element Msg
networkModeElement maybeValidModel =
    Element.el
        [ Element.Font.size 18
        , Element.Font.color <| Element.rgb 0.8 0.8 1
        , Element.Font.semiBold
        , Element.Font.italic
        , Element.centerX
        ]
        (Element.text (networkModeText maybeValidModel))


networkModeText : Model -> String
networkModeText maybeValidModel =
    case maybeValidModel of
        Failed _ ->
            "plz refresh X_X"

        Running model ->
            case model.web3Context.factoryType of
                Native Eth ->
                    "Mainnet ETH"

                Native Kovan ->
                    "Testnet ETH"

                Native Rootstock ->
                    "Rootstock SBTC"

                Native RootstockTest ->
                    "RskTest SBTC"

                Native XDai ->
                    "xDai"

                Token EthDai ->
                    "Mainnet Dai"

                Token KovanDai ->
                    "Testnet Dai"


type HeaderLinkStyle
    = Normal
    | Active
    | Important


headerLink : String -> Msg -> HeaderLinkStyle -> Element Msg
headerLink title onClick style =
    let
        extraStyles =
            case style of
                Normal ->
                    []

                Active ->
                    [ Element.Border.rounded 4
                    , Element.Background.color <| Element.rgb 0 0 1
                    ]

                Important ->
                    [ Element.Border.rounded 4
                    , Element.Background.color <| Element.rgb 0.9 0 0
                    ]
    in
    Element.el
        ([ Element.paddingXY 23 12
         , Element.Font.size 22
         , Element.Font.semiBold
         , Element.Font.color EH.white
         , Element.pointer
         , Element.Events.onClick onClick
         ]
            ++ extraStyles
        )
        (Element.text title)


logoElement : Element Msg
logoElement =
    Element.el
        [ Element.Font.size 29
        , Element.Font.color EH.white
        , Element.Font.bold
        , Element.centerX
        , Element.pointer
        , Element.Events.onClick <| GotoRoute Routing.Home
        ]
        (Element.paragraph []
            [ Element.text "DAI"
            , Element.el [ Element.Font.color EH.red ] <| Element.text "Hard"
            ]
        )


dropdownStyles : List (Attribute Msg)
dropdownStyles =
    [ Element.padding 10
    , Element.spacing 10
    , Element.Border.rounded 4
    , Element.Background.color <| Element.rgb 0.5 0.5 1
    ]


myOffersElement =
    Element.el
        headerMenuAttributes
        (Element.text "My Trades")


headerMenuAttributes : List (Attribute Msg)
headerMenuAttributes =
    [ Element.Font.size 19
    , Element.Font.color EH.white
    , Element.Font.semiBold
    , Element.padding 20
    , Element.pointer
    ]


subModelElement : Model -> Element Msg
subModelElement maybeValidModel =
    Element.el
        [ Element.width Element.fill
        , Element.height Element.fill
        , Element.Border.rounded 10
        ]
        (case maybeValidModel of
            Running model ->
                case model.submodel of
                    BetaLandingPage ->
                        Landing.View.root (GotoRoute <| Routing.Marketplace Buyer)

                    CreateModel createModel ->
                        Element.map CreateMsg (Create.View.root createModel)

                    TradeModel tradeModel ->
                        Element.map TradeMsg (Trade.View.root model.time model.tradeCache tradeModel)

                    MarketplaceModel marketplaceModel ->
                        Element.map MarketplaceMsg (Marketplace.View.root model.time model.tradeCache marketplaceModel)

                    AgentHistoryModel agentHistoryModel ->
                        Element.map AgentHistoryMsg (AgentHistory.View.root model.time model.tradeCache agentHistoryModel)

            Failed errorMessageString ->
                Element.el
                    [ Element.centerX
                    , Element.centerY
                    ]
                    (Element.paragraph
                        []
                        [ Element.text errorMessageString ]
                    )
        )
